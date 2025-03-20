use crate::{
    PythonBindType,
    generator::Generator,
    structs::{InnerOptionType, InnerVecType, RustType},
};
use std::{borrow::Cow, fs, io};

macro_rules! write_str {
    ($self:ident, $s:expr) => {
        $self.push(Cow::Borrowed($s))
    };
}

macro_rules! write_fmt {
    ($self:ident, $($arg:tt)*) => {
        $self.push(Cow::Owned(format!($($arg)*)))
    };
}

pub fn generator(type_data: &[PythonBindType]) -> io::Result<()> {
    let mut file = vec![
        Cow::Borrowed("from __future__ import annotations"),
        Cow::Borrowed(""),
        Cow::Borrowed("from typing import Optional, Sequence"),
        Cow::Borrowed(""),
        Cow::Borrowed("__doc__: str"),
        Cow::Borrowed("__version__: str"),
        Cow::Borrowed(""),
        Cow::Borrowed("class InvalidFlatbuffer(ValueError): ..."),
        Cow::Borrowed(""),
    ];

    let primitive_map = [
        ("bool", "bool"),
        ("i32", "int"),
        ("u32", "int"),
        ("f32", "float"),
        ("String", "str"),
        ("u8", "int"),
        ("Vec<u8>", "bytes"),
    ];

    let mut sorted_types: Vec<&PythonBindType> = type_data.iter().collect();
    sorted_types.sort_by(|a, b| a.struct_name().cmp(b.struct_name()));

    for item in sorted_types {
        let type_name = item.struct_name();

        write_fmt!(file, "class {type_name}:");

        match item {
            PythonBindType::Union(bind) => {
                let types = bind
                    .types
                    .iter()
                    .map(|variable_info| variable_info.name.as_str())
                    .filter(|variable_name| *variable_name != "NONE")
                    .collect::<Vec<_>>();
                let default_value = types.first().unwrap();
                let union_str = types.join(" | ");

                write_fmt!(file, "    item: {union_str}");
                write_str!(file, "");
                write_str!(file, "    def __new__(");
                write_fmt!(file, "        cls, item: {union_str} = {default_value}()");
                write_str!(file, "    ): ...");
                write_str!(file, "    def __init__(");
                write_fmt!(file, "        self, item: {union_str} = {default_value}()");
                write_str!(file, "    ): ...\n");
            }
            PythonBindType::Enum(bind) => {
                for variable_info in &bind.types {
                    let variable_name = variable_info.name.as_str();
                    if variable_name == "NONE" {
                        continue;
                    }

                    let variable_type = variable_info.raw_type.as_str();
                    write_fmt!(file, "    {variable_name} = {type_name}({variable_type})");

                    if let Some(docs) = variable_info.doc_str.as_ref() {
                        write_str!(file, "    \"\"\"");

                        for line in docs {
                            write_fmt!(file, "    {line}");
                        }

                        write_str!(file, "    \"\"\"");
                    }
                }

                write_str!(file, "");
                write_str!(file, "    def __new__(cls, value: int = 0): ...");
                write_str!(file, "    def __init__(self, value: int = 0):");
                write_str!(file, "        \"\"\"");
                write_str!(file, "        :raises ValueError: If the `value` is not a valid enum value");
                write_str!(file, "        \"\"\"");
                write_str!(file, "    def __int__(self) -> int: ...");
                write_fmt!(file, "    def __eq__(self, other: {type_name}) -> bool: ...");
                write_str!(file, "    def __hash__(self) -> str: ...");
            }
            PythonBindType::Struct(bind) => {
                if let Some(docs) = bind.struct_doc_str.as_ref() {
                    write_str!(file, "    \"\"\"");

                    for line in docs {
                        write_fmt!(file, "    {line}");
                    }

                    write_str!(file, "    \"\"\"");
                }

                let mut python_types = Vec::new();

                'outer: for variable_info in &bind.types {
                    let variable_name = variable_info.name.as_str();
                    let variable_type = variable_info.raw_type.as_str();

                    for (rust_type, python_type) in primitive_map {
                        if variable_type == rust_type {
                            python_types.push(python_type.to_string());
                            write_fmt!(file, "    {variable_name}: {python_type}");

                            if let Some(docs) = variable_info.doc_str.as_ref() {
                                write_str!(file, "    \"\"\"");

                                for line in docs {
                                    write_fmt!(file, "    {line}");
                                }

                                write_str!(file, "    \"\"\"");
                            }

                            continue 'outer;
                        }
                    }

                    match &variable_info.rust_type {
                        RustType::Vec(InnerVecType::U8) => {
                            python_types.push("bytes".to_string());
                            write_fmt!(file, "    {variable_name}: bytes");
                        }
                        RustType::Vec(InnerVecType::Base(type_name)) => {
                            let python_type = if type_name == "bool" {
                                "bool"
                            } else if type_name == "i32" || type_name == "u32" {
                                "int"
                            } else if type_name == "f32" {
                                "float"
                            } else {
                                type_name
                            };

                            python_types.push(format!("Sequence[{python_type}]"));
                            write_fmt!(file, "    {variable_name}: Sequence[{python_type}]");
                        }
                        RustType::Vec(InnerVecType::String) => {
                            python_types.push("Sequence[str]".to_string());
                            write_fmt!(file, "    {variable_name}: Sequence[str]");
                        }
                        RustType::Vec(InnerVecType::Custom(inner_type)) => {
                            python_types.push(format!("Sequence[{inner_type}]"));
                            write_fmt!(file, "    {variable_name}: Sequence[{inner_type}]");
                        }
                        RustType::Option(InnerOptionType::String, _) => {
                            python_types.push("Optional[str]".to_string());
                            write_fmt!(file, "    {variable_name}: Optional[str]");
                        }
                        RustType::Option(InnerOptionType::BaseType, type_name) => {
                            let python_type = if type_name == "bool" {
                                "bool"
                            } else if type_name == "i32" || type_name == "u32" {
                                "int"
                            } else if type_name == "f32" {
                                "float"
                            } else {
                                type_name
                            };

                            python_types.push(format!("Optional[{python_type}]"));
                            write_fmt!(file, "    {variable_name}: Optional[{python_type}]");
                        }
                        RustType::Option(InnerOptionType::Custom, type_name)
                        | RustType::Option(InnerOptionType::Box, type_name) => {
                            write_fmt!(file, "    {variable_name}: Optional[{type_name}]");

                            let python_type = if type_name == "Float" {
                                "Float | float"
                            } else if type_name == "Bool" {
                                "Bool | bool"
                            } else {
                                type_name.as_str()
                            };

                            python_types.push(format!("Optional[{python_type}]"));
                        }
                        RustType::Box(inner_type) => {
                            python_types.push(inner_type.to_string());
                            write_fmt!(file, "    {variable_name}: {inner_type}");
                        }
                        RustType::String => {
                            python_types.push("str".to_string());
                            write_fmt!(file, "    {variable_name}: str");
                        }
                        RustType::Union(type_name, is_optional) => {
                            if *is_optional {
                                write_fmt!(file, "    {variable_name}: Optional[{type_name}]");
                            } else {
                                write_fmt!(file, "    {variable_name}: {type_name}");
                            }

                            // search for the union with the name `type_name` and get the types
                            let union_types = type_data
                                .iter()
                                .find_map(|item| match item {
                                    PythonBindType::Union(bind) if bind.struct_name() == type_name => {
                                        Some(bind.types.iter().skip(1).map(|v| v.name.as_str()).collect::<Vec<_>>())
                                    }
                                    _ => None,
                                })
                                .unwrap();

                            let python_type = union_types.join(" | ");
                            python_types.push(if *is_optional {
                                format!("Optional[{python_type}]")
                            } else {
                                python_type
                            });
                        }
                        RustType::Custom(type_name) | RustType::Other(type_name) | RustType::Base(type_name) => {
                            python_types.push(type_name.to_string());
                            write_fmt!(file, "    {variable_name}: {type_name}");
                        }
                    }

                    if let Some(docs) = variable_info.doc_str.as_ref() {
                        write_str!(file, "    \"\"\"");

                        for line in docs {
                            write_fmt!(file, "    {line}");
                        }

                        write_str!(file, "    \"\"\"");
                    }
                }

                if !bind.types.is_empty() {
                    write_str!(file, "");
                    write_str!(file, "    __match_args__ = (");

                    for variable_info in &bind.types {
                        write_fmt!(file, "        \"{}\",", variable_info.name);
                    }
                    write_str!(file, "    )");
                }

                if bind.types.is_empty() {
                    write_str!(file, "    def __init__(self): ...");
                } else {
                    write_str!(file, "");

                    let inits = [("new", "cls"), ("init", "self")];

                    for (func, first_arg) in inits {
                        write_fmt!(file, "    def __{func}__(");
                        write_fmt!(file, "        {first_arg},");

                        for (variable_info, python_type) in bind.types.iter().zip(&python_types) {
                            let variable_name = variable_info.name.as_str();

                            if let Some((field, value)) = bind.default_override {
                                if field == variable_name {
                                    write_fmt!(file, "        {variable_name}: {python_type} = {value},");
                                    continue;
                                }
                            }

                            let default_value = match variable_info.raw_type.as_str() {
                                "bool" => Cow::Borrowed("False"),
                                "i32" | "u32" | "f32" | "u8" => Cow::Borrowed("0"),
                                "String" => Cow::Borrowed("\"\""),
                                "Vec<u8>" => Cow::Borrowed("b\"\""),
                                t => {
                                    if python_type.starts_with("Optional") || t.starts_with("Option<") {
                                        Cow::Borrowed("None")
                                    } else if let Some(pos) = python_type.find('|') {
                                        Cow::Owned(format!("{}()", &python_type[..pos - 1]))
                                    } else if t.starts_with("Vec<") {
                                        Cow::Borrowed("[]")
                                    } else if t.starts_with("Box<") {
                                        let inner_type =
                                            t.trim_start_matches("Box<").trim_end_matches('>').trim_end_matches('T');
                                        Cow::Owned(format!("{inner_type}()"))
                                    } else {
                                        Cow::Owned(format!("{}()", t.trim_end_matches('T')))
                                    }
                                }
                            };

                            write_fmt!(file, "        {variable_name}: {python_type} = {default_value},");
                        }

                        write_str!(file, "    ): ...");
                    }
                }

                write_str!(file, "    def pack(self) -> bytes:");
                write_str!(file, "        \"\"\"");
                write_str!(file, "        Serializes this instance into a byte array");
                write_str!(file, "        \"\"\"");

                if !bind.is_frozen {
                    write_str!(file, "    def unpack_with(self, data: bytes) -> None:");
                    write_str!(file, "        \"\"\"");
                    write_str!(file, "        Deserializes the data into this instance\n");
                    write_str!(
                        file,
                        "        :raises InvalidFlatbuffer: If the `data` is invalid for this type"
                    );
                    write_str!(file, "        \"\"\"");
                }

                write_str!(file, "    @staticmethod");
                write_fmt!(file, "    def unpack(data: bytes) -> {type_name}:");
                write_str!(file, "        \"\"\"");
                write_str!(file, "        Deserializes the data into a new instance\n");
                write_str!(
                    file,
                    "        :raises InvalidFlatbuffer: If the `data` is invalid for this type"
                );
                write_str!(file, "        \"\"\"");
            }
        }

        write_str!(file, "    def __str__(self) -> str: ...");
        write_str!(file, "    def __repr__(self) -> str: ...");
        write_str!(file, "");
    }

    fs::write("rlbot_flatbuffers.pyi", file.join("\n"))?;

    Ok(())
}
