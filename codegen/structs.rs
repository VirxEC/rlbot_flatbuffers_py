use crate::{PythonBindType, generator::Generator};
use std::{borrow::Cow, fs, iter::repeat_n, path::Path};

#[derive(Debug, PartialEq, Eq)]
pub enum InnerVecType {
    U8,
    Base(String),
    String,
    Custom(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum InnerOptionType {
    Box,
    String,
    BaseType,
    Custom,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RustType {
    Vec(InnerVecType),
    String,
    Box(String),
    Option(InnerOptionType, String),
    Union(String, bool),
    Custom(String),
    Base(String),
    Other(String),
}

pub enum SpecialBase {
    FloatT,
    BoolT,
}

pub struct CustomType {
    pub name: String,
    pub raw_type: String,
    pub rust_type: RustType,
    pub is_frozen: bool,
    pub is_special_base: Option<SpecialBase>,
    pub snake_case_name: String,
    pub doc_str: Option<Vec<String>>,
}

pub struct StructBindGenerator {
    pub filename: String,
    pub struct_name: String,
    struct_t_name: String,
    pub struct_doc_str: Option<Vec<String>>,
    pub types: Vec<CustomType>,
    file_contents: Vec<Cow<'static, str>>,
    has_complex_pack: bool,
    pub is_frozen: bool,
    is_no_set: bool,
    pub default_override: Option<(&'static str, &'static str)>,
    freelist_size: usize,
}

macro_rules! write_str {
    ($self:ident, $s:expr) => {
        $self.file_contents.push(Cow::Borrowed($s))
    };
}

macro_rules! write_fmt {
    ($self:ident, $($arg:tt)*) => {
        $self.file_contents.push(Cow::Owned(format!($($arg)*)))
    };
}

impl StructBindGenerator {
    pub fn new(
        filename: String,
        struct_name: String,
        struct_t_name: String,
        struct_doc_str: Option<Vec<String>>,
        contents: String,
        types: Vec<CustomType>,
    ) -> Option<Self> {
        let is_frozen = PythonBindType::FROZEN_TYPES.contains(&struct_name.as_str());
        let is_no_set = PythonBindType::NO_SET_TYPES.contains(&struct_name.as_str());

        let has_complex_pack = contents.contains("pub fn pack<'b, A: flatbuffers::Allocator + 'b>(");

        let mut file_contents = vec![];

        file_contents.push(Cow::Borrowed(if types.is_empty() {
            "use crate::{flat_err_to_py, generated::rlbot::flat, FromGil};"
        } else if is_frozen {
            "use crate::{flat_err_to_py, generated::rlbot::flat, FromGil, IntoGil, PyDefault};"
        } else {
            "use crate::{flat_err_to_py, generated::rlbot::flat, FromGil, IntoGil, PyDefault, UnpackFrom};"
        }));

        if has_complex_pack {
            file_contents.push(Cow::Borrowed("use flatbuffers::{root, FlatBufferBuilder};"));
            file_contents.push(Cow::Borrowed("use get_size::GetSize;"));
        } else {
            file_contents.push(Cow::Borrowed("use flatbuffers::root;"));
        }

        file_contents.push(Cow::Borrowed("use pyo3::{prelude::*, types::*};"));
        file_contents.push(Cow::Borrowed(""));

        let default_override = PythonBindType::DEFAULT_OVERRIDES.iter().find_map(|&(name, field, value)| {
            if name == struct_name.as_str() {
                Some((field, value))
            } else {
                None
            }
        });

        let freelist_size = PythonBindType::FREELIST_TYPES
            .iter()
            .find_map(|&(name, size)| if name == struct_name.as_str() { Some(size) } else { None })
            .unwrap_or_default();

        Some(Self {
            filename,
            struct_name,
            struct_t_name,
            struct_doc_str,
            types,
            file_contents,
            has_complex_pack,
            is_frozen,
            is_no_set,
            default_override,
            freelist_size,
        })
    }

    pub fn get_types(contents: &str, struct_t_name: &str) -> Option<Vec<CustomType>> {
        // find the struct definition
        let struct_start_definition = format!("pub struct {struct_t_name} {{\n");
        let struct_start = contents.find(&struct_start_definition)?;

        let struct_end_definition = "}\n";
        let struct_end = contents[struct_start..].find(struct_end_definition).unwrap();

        let start = struct_start + struct_start_definition.len();
        let end = struct_start + struct_end - struct_end_definition.len();

        if end <= start {
            return Some(Vec::new());
        }

        let struct_definition = &contents[start..end];

        let raw_types: Vec<_> = struct_definition
            .split('\n')
            .filter_map(|s| {
                let (name, raw_type) = s
                    .trim_start_matches(' ')
                    .trim_start_matches("pub ")
                    .trim_end_matches(',')
                    .split_once(": ")?;

                let var_def = format!("pub fn {name}(");
                let var_def_pos = contents.find(&var_def).unwrap();

                let mut docs = Vec::new();

                for line in contents[..var_def_pos].lines().rev().skip(2) {
                    let line = line.trim();
                    if line.starts_with("///") {
                        docs.push(line.trim_start_matches("///").trim());
                    } else {
                        break;
                    }
                }

                let struct_doc_str = if docs.is_empty() {
                    None
                } else {
                    Some(docs.into_iter().map(|s| s.to_string()).rev().collect::<Vec<_>>())
                };

                Some((name, raw_type, struct_doc_str))
            })
            .collect();

        let custom_types = Self::raw_types_to_custom(raw_types);

        Some(custom_types)
    }

    fn raw_types_to_custom(raw_types: Vec<(&str, &str, Option<Vec<String>>)>) -> Vec<CustomType> {
        raw_types
            .into_iter()
            .map(|(name, raw_type, doc_str)| {
                let (rust_type, inner_type) = if raw_type.starts_with("Vec<") {
                    if raw_type == "Vec<u8>" {
                        (RustType::Vec(InnerVecType::U8), None)
                    } else if raw_type == "Vec<String>" {
                        (RustType::Vec(InnerVecType::String), None)
                    } else {
                        let inner_type = raw_type
                            .trim_start_matches("Vec<")
                            .trim_end_matches('>')
                            .trim_end_matches('T');

                        (
                            RustType::Vec(if PythonBindType::BASE_TYPES.contains(&inner_type) {
                                InnerVecType::Base(inner_type.to_string())
                            } else {
                                InnerVecType::Custom(inner_type.to_string())
                            }),
                            Some(inner_type),
                        )
                    }
                } else if raw_type.starts_with("Option<") {
                    let inner = raw_type.trim_start_matches("Option<").trim_end_matches('>');

                    if inner.starts_with("Box<") {
                        let inner_type = inner.trim_start_matches("Box<").trim_end_matches('>').trim_end_matches('T');
                        (
                            RustType::Option(InnerOptionType::Box, inner_type.to_string()),
                            Some(inner_type),
                        )
                    } else if inner == "String" {
                        (RustType::Option(InnerOptionType::String, inner.to_string()), None)
                    } else if PythonBindType::BASE_TYPES.contains(&inner) {
                        (RustType::Option(InnerOptionType::BaseType, inner.to_string()), None)
                    } else {
                        let inner = inner.trim_end_matches('T');
                        (RustType::Option(InnerOptionType::Custom, inner.to_string()), Some(inner))
                    }
                } else if raw_type == "String" {
                    (RustType::String, None)
                } else if raw_type.starts_with("Box<") {
                    let inner_type = raw_type
                        .trim_start_matches("Box<")
                        .trim_end_matches('>')
                        .trim_end_matches('T');
                    (RustType::Box(inner_type.to_string()), Some(inner_type))
                } else if raw_type.ends_with('T') {
                    let inner_type = raw_type.trim_end_matches('T');

                    if PythonBindType::UNIONS.contains(&inner_type) {
                        (
                            RustType::Union(inner_type.to_string(), PythonBindType::OPTIONAL_UNIONS.contains(&inner_type)),
                            Some(inner_type),
                        )
                    } else {
                        (RustType::Custom(inner_type.to_string()), Some(inner_type))
                    }
                } else if PythonBindType::BASE_TYPES.contains(&raw_type) {
                    (RustType::Base(raw_type.to_string()), None)
                } else {
                    (RustType::Other(raw_type.to_string()), Some(raw_type))
                };

                let (is_frozen, is_special_base) = if let Some(inner_type) = inner_type {
                    let is_frozen = PythonBindType::FROZEN_TYPES.contains(&inner_type);
                    let is_special_base = if inner_type == "Float" {
                        Some(SpecialBase::FloatT)
                    } else if inner_type == "Bool" {
                        Some(SpecialBase::BoolT)
                    } else {
                        None
                    };

                    (is_frozen, is_special_base)
                } else {
                    (false, None)
                };

                CustomType {
                    name: name.to_string(),
                    raw_type: raw_type.to_string(),
                    rust_type,
                    is_frozen,
                    is_special_base,
                    snake_case_name: String::new(),
                    doc_str,
                }
            })
            .collect()
    }

    fn generate_new_method(&mut self) {
        write_str!(self, "    #[new]");

        if self.types.is_empty() {
            write_str!(self, "    pub fn new() -> Self {");
            write_str!(self, "        Self {}");
            write_str!(self, "    }");
            return;
        }

        let mut signature_parts = Vec::new();
        let mut needs_python = false;

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if let Some((field, value)) = self.default_override {
                if field == variable_name {
                    signature_parts.push(format!("{variable_name}={value}"));
                    continue;
                }
            }

            let sig_part = match &variable_info.rust_type {
                RustType::Option(_, _) => {
                    if variable_info.is_special_base.is_some() {
                        needs_python = true;
                    }

                    format!("{variable_name}=None")
                }
                RustType::Union(_, _)
                | RustType::Box(_)
                | RustType::Custom(_)
                | RustType::Vec(InnerVecType::U8)
                | RustType::Vec(InnerVecType::Custom(_))
                | RustType::String => {
                    needs_python = true;
                    format!("{variable_name}=None")
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        needs_python = true;
                        format!("{variable_name}=Default::default()")
                    }
                    _ => format!("{variable_name}=Default::default()"),
                },
                _ => {
                    format!("{variable_name}=Default::default()")
                }
            };

            signature_parts.push(sig_part);
        }

        let max_num_types = if needs_python { 6 } else { 7 };
        if self.types.len() > max_num_types {
            write_str!(self, "    #[allow(clippy::too_many_arguments)]");
        }

        write_fmt!(self, "    #[pyo3(signature = ({}))]", signature_parts.join(", "));
        write_str!(self, "    pub fn new(");

        if needs_python {
            write_str!(self, "        py: Python,");
        }

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            let variable_type = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::Custom(_)) => Cow::Borrowed("Option<Py<PyList>>"),
                RustType::Vec(InnerVecType::Base(inner_type)) => Cow::Owned(format!("Vec<{}>", inner_type)),
                RustType::Vec(InnerVecType::String) => Cow::Borrowed("Vec<String>"),
                RustType::Vec(InnerVecType::U8) => Cow::Borrowed("Option<Py<PyBytes>>"),
                RustType::Box(inner_type) => Cow::Owned(format!("Option<Py<super::{inner_type}>>")),
                RustType::Option(InnerOptionType::BaseType, inner_type) => Cow::Owned(format!("Option<{inner_type}>")),
                RustType::Option(InnerOptionType::String, _) => Cow::Borrowed("Option<Py<PyString>>"),
                RustType::Option(_, inner_type) => {
                    if inner_type == "Float" {
                        Cow::Borrowed("Option<crate::PartFloats>")
                    } else if inner_type == "Bool" {
                        Cow::Borrowed("Option<crate::PartBools>")
                    } else {
                        Cow::Owned(format!("Option<Py<super::{inner_type}>>"))
                    }
                }
                RustType::Base(inner_type) => Cow::Borrowed(match inner_type.as_str() {
                    "f32" => "f64",
                    item => item,
                }),
                RustType::String => Cow::Borrowed("Option<Py<PyString>>"),
                RustType::Union(inner_type, _) => Cow::Owned(format!("Option<super::{inner_type}Union>")),
                RustType::Custom(inner_type) => Cow::Owned(format!("Option<Py<super::{inner_type}>>")),
                RustType::Other(inner_type) => Cow::Owned(format!("super::{inner_type}")),
            };

            write_fmt!(self, "        {variable_name}: {variable_type},");
        }

        write_str!(self, "    ) -> Self {");
        write_str!(self, "        Self {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.is_special_base.is_some() {
                write_fmt!(self, "            {variable_name}: {variable_name}.map(|x| x.into_gil(py)),");
                continue;
            }

            match &variable_info.rust_type {
                RustType::Union(inner_type, is_optional) => {
                    let end = if *is_optional {
                        Cow::Borrowed("")
                    } else {
                        Cow::Owned(format!(".unwrap_or_else(|| super::{inner_type}::py_default(py))"))
                    };

                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.map(|u| Py::new(py, super::{inner_type}::new(u)).unwrap()){end},"
                    );
                }
                RustType::Box(inner_type) | RustType::Custom(inner_type) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| super::{inner_type}::py_default(py)),"
                    );
                }
                RustType::Vec(InnerVecType::U8) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| PyBytes::new(py, &[]).unbind()),"
                    );
                }
                RustType::Vec(InnerVecType::Custom(_)) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| PyList::empty(py).unbind()),"
                    );
                }
                RustType::String => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| crate::pydefault_string(py)),"
                    );
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => write_fmt!(
                        self,
                        "            {variable_name}: PyFloat::new(py, {variable_name}).unbind(),"
                    ),
                    _ => write_fmt!(self, "            {variable_name},"),
                },
                _ => write_fmt!(self, "            {variable_name},"),
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");

        if self.is_frozen || self.is_no_set {
            return;
        }

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            match &variable_info.rust_type {
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        write_str!(self, "\n    #[setter]");
                        write_fmt!(self, "    pub fn {variable_name}(&mut self, py: Python, value: f64) {{",);
                        write_fmt!(self, "        self.{variable_name} = PyFloat::new(py, value).unbind();");
                        write_str!(self, "    }");
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }
    }

    fn generate_str_method(&mut self) {
        write_str!(self, "    pub fn __str__(&self, py: Python) -> String {");
        write_str!(self, "        self.__repr__(py)");
        write_str!(self, "    }");
    }

    fn generate_repr_method(&mut self) {
        if self.types.is_empty() {
            write_str!(self, "    pub fn __repr__(&self, _py: Python) -> String {");
            write_fmt!(self, "        String::from(\"{}()\")", self.struct_name);
            write_str!(self, "    }");
            return;
        }

        write_str!(self, "    #[allow(unused_variables)]");
        write_str!(self, "    pub fn __repr__(&self, py: Python) -> String {");
        write_str!(self, "        format!(");

        let repr_signature = self
            .types
            .iter()
            .map(|variable_info| {
                let variable_name = variable_info.name.as_str();

                match &variable_info.rust_type {
                    RustType::String => format!("{variable_name}={{:?}}"),
                    RustType::Vec(InnerVecType::U8) => format!("{variable_name}=bytes([{{}}])"),
                    RustType::Vec(_) => format!("{variable_name}=[{{}}]"),
                    _ => format!("{variable_name}={{}}"),
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        write_fmt!(self, "            \"{}({repr_signature})\",", self.struct_name);

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            match &variable_info.rust_type {
                RustType::Vec(inner_type) => {
                    write_fmt!(self, "            self.{variable_name}");
                    match inner_type {
                        InnerVecType::U8 => {
                            write_str!(self, "                .as_bytes(py)");
                            write_str!(self, "                .iter()");
                            write_str!(self, "                .map(ToString::to_string)");
                        }
                        InnerVecType::String => {
                            write_str!(self, "                .iter()");
                            write_str!(self, "                .map(|s| format!(\"{s:?}\"))");
                        }
                        InnerVecType::Base(_) => {
                            write_str!(self, "                .iter()");
                            write_str!(self, "                .map(ToString::to_string)");
                        }
                        InnerVecType::Custom(type_name) => {
                            write_str!(self, "                .bind_borrowed(py)");
                            write_str!(self, "                .iter()");
                            write_fmt!(
                                self,
                                "                .map(|x| x.downcast_into::<super::{type_name}>().unwrap().borrow().__repr__(py))"
                            );
                        }
                    }
                    write_str!(self, "                .collect::<Vec<String>>()");
                    write_str!(self, "                .join(\", \"),");
                }
                RustType::Option(inner_type, _) => {
                    write_fmt!(self, "            self.{variable_name}");
                    write_str!(self, "                .as_ref()");
                    write_str!(self, "                .map_or_else(crate::none_str, |i| {");

                    match inner_type {
                        InnerOptionType::BaseType => {
                            write_str!(self, "                    format!(\"{i:?}\")");
                        }
                        InnerOptionType::String => {
                            write_str!(
                                self,
                                "                    format!(\"{:?}\", i.to_str(py).unwrap().to_string())"
                            );
                        }
                        _ => {
                            write_str!(self, "                    i.borrow(py).__repr__(py)");
                        }
                    }

                    write_str!(self, "                }),");
                }
                RustType::Union(_, is_optional) => {
                    if *is_optional {
                        write_fmt!(self, "            self.{variable_name}");
                        write_str!(self, "                .as_ref()");
                        write_str!(
                            self,
                            "                .map_or_else(crate::none_str, |i| i.borrow(py).inner_repr(py)),"
                        );
                    } else {
                        write_fmt!(self, "            self.{variable_name}.borrow(py).inner_repr(py),");
                    }
                }
                RustType::Box(_) | RustType::Custom(_) => {
                    write_fmt!(self, "            self.{variable_name}.borrow(py).__repr__(py),");
                }
                RustType::Base(inner_type) => {
                    if inner_type == "bool" {
                        write_fmt!(self, "            crate::bool_to_str(self.{variable_name}),");
                    } else {
                        write_fmt!(self, "            self.{variable_name},");
                    }
                }
                RustType::String => {
                    write_fmt!(self, "            self.{variable_name}.bind(py).to_cow().unwrap(),");
                }
                RustType::Other(_) => {
                    write_fmt!(self, "            self.{variable_name}.__repr__(),");
                }
            }
        }

        write_str!(self, "        )");
        write_str!(self, "    }");
    }

    fn generate_long_args(&mut self) {
        write_str!(self, "    #[classattr]");
        write_str!(self, "    fn __match_args__(py: Python) -> Bound<pyo3::types::PyTuple> {");
        write_str!(self, "        pyo3::types::PyTuple::new(py, [");

        for variable_info in &self.types {
            write_fmt!(self, "            \"{}\",", variable_info.name);
        }

        write_str!(self, "        ]).unwrap()");
        write_str!(self, "    }\n");
        write_str!(self, "    #[classattr]");
        write_str!(self, "    fn __slots__(py: Python) -> Bound<pyo3::types::PyTuple> {");
        write_str!(self, "        Self::__match_args__(py)");
        write_str!(self, "    }\n");
    }

    fn generate_args(&mut self) {
        if self.types.is_empty() {
            return;
        }

        if self.types.len() > 12 {
            self.generate_long_args();
            return;
        }

        let sig_parts: Vec<_> = repeat_n("&'static str", self.types.len()).collect();
        let sig = sig_parts.join(", ");

        write_str!(self, "    #[classattr]");
        write_fmt!(self, "    fn __match_args__() -> ({sig},) {{");
        write_str!(self, "        (");

        for variable_info in &self.types {
            write_fmt!(self, "            \"{}\",", variable_info.name);
        }

        write_str!(self, "        )");
        write_str!(self, "    }\n");
        write_str!(self, "    #[classattr]");
        write_fmt!(self, "    fn __slots__() -> ({sig},) {{");
        write_str!(self, "        Self::__match_args__()");
        write_str!(self, "    }\n");
    }

    fn generate_pack_method(&mut self) {
        write_str!(self, "    fn pack<'py>(&self, py: Python<'py>) -> Bound<'py, PyBytes> {");
        write_fmt!(
            self,
            "        let flat_t = flat::{}::from_gil(py, self);",
            &self.struct_t_name
        );

        if self.has_complex_pack {
            write_str!(self, "        let size = flat_t.get_size().next_power_of_two();");
            write_str!(self, "");
            write_str!(self, "        let mut builder = FlatBufferBuilder::with_capacity(size);");
            write_str!(self, "        let offset = flat_t.pack(&mut builder);");
            write_str!(self, "        builder.finish(offset, None);");
            write_str!(self, "");
            write_str!(self, "        PyBytes::new(py, builder.finished_data())");
        } else {
            write_str!(self, "        let item = flat_t.pack();");
            write_str!(self, "");
            write_str!(self, "        PyBytes::new(py, &item.0)");
        }

        write_str!(self, "    }");
    }

    fn generate_unpack_from(&mut self) {
        write_fmt!(
            self,
            "impl UnpackFrom<flat::{}> for {} {{",
            self.struct_t_name,
            self.struct_name
        );
        write_str!(self, "    #[allow(unused_variables)]");
        write_fmt!(
            self,
            "    fn unpack_from(&mut self, py: Python, flat_t: flat::{}) {{",
            self.struct_t_name
        );

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            match &variable_info.rust_type {
                RustType::Vec(InnerVecType::Custom(inner_type)) => {
                    write_fmt!(
                        self,
                        "        crate::update_list::<_, super::{inner_type}>(py, self.{variable_name}.bind_borrowed(py), flat_t.{variable_name});",
                    );
                }
                RustType::Vec(InnerVecType::U8) => {
                    write_fmt!(
                        self,
                        "        self.{variable_name} = PyBytes::new(py, &flat_t.{variable_name}).unbind();"
                    );
                }
                RustType::Option(InnerOptionType::Box, _) => {
                    write_fmt!(self, "        match flat_t.{variable_name} {{");
                    write_str!(self, "            Some(x) => {");
                    write_fmt!(self, "                match &mut self.{variable_name} {{");
                    write_str!(
                        self,
                        "                    Some(item) => item.bind_borrowed(py).borrow_mut().unpack_from(py, *x),"
                    );
                    write_fmt!(
                        self,
                        "                    None => self.{variable_name} = Some(crate::into_py_from(py, *x)),"
                    );
                    write_str!(self, "                }");
                    write_str!(self, "            }");
                    write_fmt!(self, "            None => self.{variable_name} = None,");
                    write_str!(self, "        }");
                }
                RustType::Option(InnerOptionType::Custom, _) => {
                    write_fmt!(self, "        match flat_t.{variable_name} {{");
                    write_str!(self, "            Some(x) => {");
                    write_fmt!(self, "                match &mut self.{variable_name} {{");
                    write_str!(
                        self,
                        "                    Some(item) => item.bind_borrowed(py).borrow_mut().unpack_from(py, x),"
                    );
                    write_fmt!(
                        self,
                        "                    None => self.{variable_name} = Some(crate::into_py_from(py, x)),"
                    );
                    write_str!(self, "                }");
                    write_str!(self, "            }");
                    write_fmt!(self, "            None => self.{variable_name} = None,");
                    write_str!(self, "        }");
                }
                RustType::Option(InnerOptionType::String, _) => {
                    write_fmt!(
                        self,
                        "        self.{variable_name} = flat_t.{variable_name}.map(|s| PyString::new(py, &s).unbind());"
                    );
                }
                RustType::Box(_) => {
                    write_fmt!(
                        self,
                        "        self.{variable_name}.bind_borrowed(py).borrow_mut().unpack_from(py, *flat_t.{variable_name});"
                    )
                }
                RustType::Custom(_) => {
                    write_fmt!(
                        self,
                        "        self.{variable_name}.bind_borrowed(py).borrow_mut().unpack_from(py, flat_t.{variable_name});"
                    )
                }
                RustType::Union(inner_type, true) => {
                    write_fmt!(self, "        match flat_t.{variable_name} {{");
                    write_fmt!(self, "            flat::{inner_type}T::NONE => self.{variable_name} = None,");
                    write_str!(self, "            x => {");
                    write_fmt!(self, "                match &mut self.{variable_name} {{");
                    write_str!(
                        self,
                        "                    Some(item) => item.bind_borrowed(py).borrow_mut().unpack_from(py, x),"
                    );
                    write_fmt!(
                        self,
                        "                    None => self.{variable_name} = Some(crate::into_py_from(py, x)),"
                    );
                    write_str!(self, "                }");
                    write_str!(self, "            }");
                    write_str!(self, "        }");
                }
                RustType::Union(_, false) => {
                    let conv_str = if variable_info.is_frozen {
                        " = crate::into_py_from"
                    } else {
                        ".bind_borrowed(py).borrow_mut().unpack_from"
                    };

                    write_fmt!(self, "        self.{variable_name}{conv_str}(py, flat_t.{variable_name});");
                }
                RustType::String => {
                    write_fmt!(
                        self,
                        "        self.{variable_name} = PyString::new(py, &flat_t.{variable_name}).unbind();"
                    );
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        write_fmt!(
                            self,
                            "        self.{variable_name} = crate::float_to_py(py, flat_t.{variable_name});"
                        );
                    }
                    _ => {
                        write_fmt!(self, "        self.{variable_name} = flat_t.{variable_name};");
                    }
                },
                RustType::Other(_) => {
                    write_fmt!(self, "        self.{variable_name} = flat_t.{variable_name}.into();",);
                }
                _ => write_fmt!(self, "        self.{variable_name} = flat_t.{variable_name};"),
            }
        }

        write_str!(self, "    }");
        write_str!(self, "}");
    }

    fn generate_unpack_with(&mut self) {
        write_fmt!(
            self,
            "    fn unpack_with(&mut self, py: Python, data: &[u8]) -> PyResult<()> {{"
        );
        write_fmt!(self, "        match root::<flat::{}>(data) {{", self.struct_name);
        write_str!(self, "            Ok(flat_t) => {");
        write_str!(self, "                self.unpack_from(py, flat_t.unpack());");
        write_str!(self, "                Ok(())");
        write_str!(self, "            }");
        write_str!(self, "            Err(e) => Err(flat_err_to_py(e)),");
        write_str!(self, "        }");
        write_str!(self, "    }");
    }

    fn generate_unpack_method(&mut self) {
        write_str!(self, "    #[staticmethod]");
        write_str!(self, "    fn unpack(py: Python, data: &[u8]) -> PyResult<Py<Self>> {");
        write_fmt!(self, "        match root::<flat::{}>(data) {{", self.struct_name);
        write_str!(
            self,
            "            Ok(flat_t) => Ok(crate::into_py_from(py, flat_t.unpack())),"
        );
        write_str!(self, "            Err(e) => Err(flat_err_to_py(e)),");
        write_str!(self, "        }");
        write_str!(self, "    }");

        if !(self.is_frozen || self.types.is_empty()) {
            write_str!(self, "");
            self.generate_unpack_with();
        }
    }
}

impl Generator for StructBindGenerator {
    fn filename(&self) -> &str {
        &self.filename
    }

    fn struct_name(&self) -> &str {
        &self.struct_name
    }

    fn file_contents(&self) -> &Vec<Cow<'static, str>> {
        &self.file_contents
    }

    fn modify_source(&self, path: &Path) {
        let mut contents = fs::read_to_string(path).unwrap();

        #[cfg(windows)]
        {
            contents = contents.replace("\r\n", "\n");
        }

        contents = contents.replace("use self::flatbuffers", "use get_size::GetSize;\nuse self::flatbuffers");

        contents = contents.replace("#[derive(Debug, Clone, PartialEq)]\n", "#[derive(PartialEq, GetSize)]\n");

        contents = contents.replace(
            "#[derive(Debug, Clone, PartialEq, Default)]\n",
            "#[derive(PartialEq, Default, GetSize)]\n",
        );

        // delete unneeded auto-generated source code from flatc
        let start = contents.find("impl core::fmt::Debug for ").unwrap();
        let end = contents[start..].find("\n}").unwrap() + 3;
        contents.replace_range(start..start + end, "");

        if let Some(start) = contents.find("impl<'a> Default for ") {
            let end = contents[start..].find("\n}").unwrap() + 3;
            contents.replace_range(start..start + end, "");
        }

        fs::write(path, contents).unwrap();
    }

    fn generate_definition(&mut self) {
        let freelist_str = if self.freelist_size > 0 {
            Cow::Owned(format!(", freelist = {}", self.freelist_size))
        } else {
            Cow::Borrowed("")
        };

        let pyclass_start_str = "#[pyclass(module = \"rlbot_flatbuffers\", subclass, ";
        if self.is_frozen {
            write_fmt!(self, "{pyclass_start_str}get_all, frozen{freelist_str})]");
        } else if self.types.is_empty() {
            write_fmt!(self, "{pyclass_start_str}frozen{freelist_str})]");
        } else {
            write_fmt!(self, "{pyclass_start_str}get_all{freelist_str})]");
        }

        if self.types.is_empty() {
            write_str!(self, "#[derive(Default)]");
            write_fmt!(self, "pub struct {} {{}}", self.struct_name);
            write_str!(self, "");
            return;
        }

        let gen_set = !(self.is_no_set || self.is_frozen);

        write_fmt!(self, "pub struct {} {{", self.struct_name);

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            let mut add_set = true;
            let variable_type = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => String::from("Py<PyBytes>"),
                RustType::Vec(InnerVecType::String) => String::from("Vec<String>"),
                RustType::Vec(InnerVecType::Base(inner_type)) => format!("Vec<{}>", inner_type),
                RustType::Vec(InnerVecType::Custom(_)) => String::from("Py<PyList>"),
                RustType::Box(inner_type) => format!("Py<super::{inner_type}>"),
                RustType::Option(InnerOptionType::BaseType, inner_type) => {
                    format!("Option<Py<{inner_type}>>")
                }
                RustType::Option(InnerOptionType::String, _) => String::from("Option<Py<PyString>>"),
                RustType::Union(inner_type, true) | RustType::Option(_, inner_type) => {
                    format!("Option<Py<super::{inner_type}>>")
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        add_set = false;
                        String::from("Py<PyFloat>")
                    }
                    _ => inner_type.clone(),
                },
                RustType::String => String::from("Py<PyString>"),
                RustType::Union(inner_type, false) | RustType::Custom(inner_type) => {
                    format!("Py<super::{inner_type}>")
                }
                RustType::Other(inner_type) => format!("super::{inner_type}"),
            };

            if gen_set && add_set {
                write_str!(self, "    #[pyo3(set)]");
            }

            write_fmt!(self, "    pub {variable_name}: {variable_type},");
        }

        write_str!(self, "}");
        write_str!(self, "");

        write_fmt!(self, "impl crate::PyDefault for {} {{", self.struct_name);
        write_str!(self, "    fn py_default(py: Python) -> Py<Self> {");
        write_str!(self, "        Py::new(py, Self {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if let Some((field, value)) = self.default_override {
                if field == variable_name {
                    write_fmt!(self, "            {variable_name}: {value},");
                    continue;
                }
            }

            let end = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => Cow::Borrowed("PyBytes::new(py, &[]).unbind()"),
                RustType::Vec(InnerVecType::Custom(_)) => Cow::Borrowed("PyList::empty(py).unbind()"),
                RustType::Vec(_) => Cow::Borrowed("Vec::new()"),
                RustType::Union(_, true) | RustType::Option(_, _) => Cow::Borrowed("None"),
                RustType::Union(inner_type, false) | RustType::Box(inner_type) | RustType::Custom(inner_type) => {
                    Cow::Owned(format!("super::{inner_type}::py_default(py)"))
                }
                RustType::Base(inner_type) => Cow::Borrowed(match inner_type.as_str() {
                    "f32" => "crate::pyfloat_default(py)",
                    _ => "Default::default()",
                }),
                RustType::String => Cow::Borrowed("crate::pydefault_string(py)"),
                RustType::Other(_) => Cow::Borrowed("Default::default()"),
            };

            write_fmt!(self, "            {variable_name}: {end},");
        }

        write_str!(self, "        }).unwrap()");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");

        if !self.is_frozen {
            self.generate_unpack_from();
            write_str!(self, "");
        }
    }

    fn generate_from_flat_impls(&mut self) {
        let impl_type = format!("flat::{}", self.struct_t_name);

        if self.types.is_empty() {
            write_fmt!(self, "impl From<{impl_type}> for {} {{", self.struct_name);
            write_fmt!(self, "    fn from(_: {impl_type}) -> Self {{");
            write_fmt!(self, "        {} {{}}", self.struct_name);
            write_str!(self, "    }");
            write_str!(self, "}");
            write_str!(self, "");
            return;
        }

        write_fmt!(self, "impl FromGil<{impl_type}> for {} {{", self.struct_name);

        write_str!(self, "    #[allow(unused_variables)]");
        write_fmt!(self, "    fn from_gil(py: Python, flat_t: {impl_type}) -> Self {{");
        write_fmt!(self, "        {} {{", self.struct_name);

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: PyBytes::new(py, &flat_t.{variable_name}).unbind(),"
                    )
                }
                RustType::Vec(InnerVecType::String | InnerVecType::Base(_)) => {
                    write_fmt!(self, "            {variable_name}: flat_t.{variable_name},")
                }
                RustType::Vec(InnerVecType::Custom(type_name)) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: PyList::new(py, flat_t.{variable_name}.into_iter().map(|x| crate::into_py_from::<_, super::{type_name}>(py, x))).unwrap().unbind(),",
                    )
                }
                RustType::Option(InnerOptionType::Box, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.map(|x| crate::into_py_from(py, *x)),"
                    );
                }
                RustType::Option(InnerOptionType::String, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.map(|s| PyString::new(py, &s).unbind()),"
                    );
                }
                RustType::Option(_, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.map(|x| crate::into_py_from(py, x)),"
                    );
                }
                RustType::Box(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: crate::into_py_from(py, *flat_t.{variable_name}),",
                    );
                }
                RustType::Union(inner_type, true) => {
                    write_fmt!(self, "            {variable_name}: match flat_t.{variable_name} {{");
                    write_fmt!(self, "                flat::{inner_type}T::NONE => None,");
                    write_str!(self, "                x => Some(crate::into_py_from(py, x)),");
                    write_str!(self, "            },");
                }
                RustType::Union(_, false) | RustType::Custom(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: crate::into_py_from(py, flat_t.{variable_name}),",
                    );
                }
                RustType::String => {
                    write_fmt!(
                        self,
                        "            {variable_name}: PyString::new(py, &flat_t.{variable_name}).unbind(),"
                    );
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        write_fmt!(
                            self,
                            "            {variable_name}: crate::float_to_py(py, flat_t.{variable_name}),"
                        );
                    }
                    _ => {
                        write_fmt!(self, "            {variable_name}: flat_t.{variable_name},");
                    }
                },
                RustType::Other(_) => {
                    write_fmt!(self, "            {variable_name}: flat_t.{variable_name}.into(),",);
                }
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_to_flat_impls(&mut self) {
        let impl_type = format!("flat::{}", self.struct_t_name);

        if self.types.is_empty() {
            write_fmt!(self, "impl From<&{}> for {impl_type} {{", self.struct_name);
            write_fmt!(self, "    fn from(_: &{}) -> Self {{", self.struct_name);
            write_str!(self, "        Self {}");
            write_str!(self, "    }");
            write_str!(self, "}");
            write_str!(self, "");
            return;
        }

        write_fmt!(self, "impl FromGil<&{}> for {impl_type} {{", self.struct_name);

        write_str!(self, "    #[allow(unused_variables)]");
        write_fmt!(self, "    fn from_gil(py: Python, py_type: &{}) -> Self {{", self.struct_name);
        write_str!(self, "        Self {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.as_bytes(py).to_vec(),"
                    )
                }
                RustType::Vec(InnerVecType::String | InnerVecType::Base(_)) => {
                    write_fmt!(self, "            {variable_name}: py_type.{variable_name}.clone(),")
                }
                RustType::Vec(InnerVecType::Custom(_)) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.bind_borrowed(py).iter().map(|x| crate::from_pyany_into(py, x)).collect(),",
                    )
                }
                RustType::Option(InnerOptionType::Box, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.as_ref().map(|x| Box::new(crate::from_py_into(py, x))),"
                    );
                }
                RustType::Option(InnerOptionType::String, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.as_ref().map(|s| s.to_str(py).unwrap().to_string()),"
                    );
                }
                RustType::Option(_, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.as_ref().map(|x| crate::from_py_into(py, x)),"
                    );
                }
                RustType::Box(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: Box::new(crate::from_py_into(py, &py_type.{variable_name})),",
                    );
                }
                RustType::Union(inner_type, true) => {
                    write_fmt!(self, "            {variable_name}: py_type");
                    write_fmt!(self, "                .{variable_name}");
                    write_str!(self, "                .as_ref()");
                    write_fmt!(self, "                .map_or(flat::{inner_type}T::NONE, |x| {{");
                    write_fmt!(
                        self,
                        "                    crate::from_py_into::<_, flat::{inner_type}T>(py, x).into()"
                    );
                    write_str!(self, "                }),");
                }
                RustType::Union(_, false) | RustType::Custom(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: crate::from_py_into(py, &py_type.{variable_name}),",
                    );
                }
                RustType::String => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.to_str(py).unwrap().to_string(),",
                    );
                }
                RustType::Base(inner_type) => match inner_type.as_str() {
                    "f32" => {
                        write_fmt!(
                            self,
                            "            {variable_name}: crate::float_from_py(py, &py_type.{variable_name}),"
                        );
                    }
                    _ => {
                        write_fmt!(self, "            {variable_name}: py_type.{variable_name},");
                    }
                },
                RustType::Other(_) => {
                    write_fmt!(self, "            {variable_name}: (&py_type.{variable_name}).into(),",);
                }
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_py_methods(&mut self) {
        write_str!(self, "#[pymethods]");
        write_fmt!(self, "impl {} {{", self.struct_name);

        self.generate_new_method();
        write_str!(self, "");

        self.generate_str_method();
        write_str!(self, "");

        self.generate_repr_method();
        write_str!(self, "");

        self.generate_args();

        self.generate_pack_method();
        write_str!(self, "");

        self.generate_unpack_method();
        write_str!(self, "}");
        write_str!(self, "");
    }
}
