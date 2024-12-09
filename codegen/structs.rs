use crate::{generator::Generator, PythonBindType};
use std::{borrow::Cow, fs, iter::repeat, path::Path};

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
    Union(String),
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
    pub frozen_needs_py: bool,
    pub is_special_base: Option<SpecialBase>,
    pub snake_case_name: String,
}

pub struct StructBindGenerator {
    pub filename: String,
    pub struct_name: String,
    struct_t_name: String,
    pub types: Vec<CustomType>,
    file_contents: Vec<Cow<'static, str>>,
    has_complex_pack: bool,
    is_all_base_types: bool,
    is_frozen: bool,
    frozen_needs_py: bool,
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
        contents: String,
        types: Vec<CustomType>,
    ) -> Option<Self> {
        let is_frozen = PythonBindType::FROZEN_TYPES.contains(&struct_name.as_str());
        let frozen_needs_py = PythonBindType::FROZEN_NEEDS_PY.contains(&struct_name.as_str());

        let is_all_base_types = types
            .iter()
            .all(|t| matches!(t.rust_type, RustType::Base(_)));
        let has_complex_pack =
            contents.contains("pub fn pack<'b, A: flatbuffers::Allocator + 'b>(");

        let mut file_contents = vec![];

        file_contents.push(Cow::Borrowed(
            if (is_frozen && !frozen_needs_py) || is_all_base_types || types.is_empty() {
                "use crate::{flat_err_to_py, generated::rlbot::flat, FromGil};"
            } else {
                "use crate::{flat_err_to_py, generated::rlbot::flat, FromGil, IntoGil, PyDefault};"
            },
        ));

        if has_complex_pack {
            file_contents.push(Cow::Borrowed("use flatbuffers::{root, FlatBufferBuilder};"));
            file_contents.push(Cow::Borrowed("use get_size::GetSize;"));
        } else {
            file_contents.push(Cow::Borrowed("use flatbuffers::root;"));
        }

        file_contents.push(Cow::Borrowed(if is_frozen {
            "use pyo3::{pyclass, pymethods, types::PyBytes, Bound, PyResult, Python};"
        } else {
            "use pyo3::{pyclass, pymethods, types::PyBytes, Bound, Py, PyResult, Python};"
        }));

        file_contents.push(Cow::Borrowed(""));

        Some(Self {
            filename,
            struct_name,
            struct_t_name,
            types,
            file_contents,
            has_complex_pack,
            is_all_base_types,
            is_frozen,
            frozen_needs_py,
        })
    }

    pub fn get_types(contents: &str, struct_t_name: &str) -> Option<Vec<CustomType>> {
        // find the struct definition
        let struct_start_definition = format!("pub struct {struct_t_name} {{\n");
        let struct_start = contents.find(&struct_start_definition)?;

        let struct_end_definition = "}\n";
        let struct_end = contents[struct_start..]
            .find(struct_end_definition)
            .unwrap();

        let start = struct_start + struct_start_definition.len();
        let end = struct_start + struct_end - struct_end_definition.len();

        if end <= start {
            return Some(Vec::new());
        }

        let struct_definition = &contents[start..end];

        let raw_types: Vec<_> = struct_definition
            .split('\n')
            .filter_map(|s| {
                s.trim_start_matches(' ')
                    .trim_start_matches("pub ")
                    .trim_end_matches(',')
                    .split_once(": ")
            })
            .collect();

        let custom_types = Self::raw_types_to_custom(raw_types);

        Some(custom_types)
    }

    fn raw_types_to_custom(raw_types: Vec<(&str, &str)>) -> Vec<CustomType> {
        raw_types
            .into_iter()
            .map(|(name, raw_type)| {
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
                        let inner_type = inner
                            .trim_start_matches("Box<")
                            .trim_end_matches('>')
                            .trim_end_matches('T');
                        (
                            RustType::Option(InnerOptionType::Box, inner_type.to_string()),
                            Some(inner_type),
                        )
                    } else if inner == "String" {
                        (
                            RustType::Option(InnerOptionType::String, inner.to_string()),
                            None,
                        )
                    } else if PythonBindType::BASE_TYPES.contains(&inner) {
                        (
                            RustType::Option(InnerOptionType::BaseType, inner.to_string()),
                            None,
                        )
                    } else {
                        let inner = inner.trim_end_matches('T');
                        (
                            RustType::Option(InnerOptionType::Custom, inner.to_string()),
                            Some(inner),
                        )
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
                        (RustType::Union(inner_type.to_string()), Some(inner_type))
                    } else {
                        (RustType::Custom(inner_type.to_string()), Some(inner_type))
                    }
                } else if PythonBindType::BASE_TYPES.contains(&raw_type) {
                    (RustType::Base(raw_type.to_string()), None)
                } else {
                    (RustType::Other(raw_type.to_string()), Some(raw_type))
                };

                let (is_frozen, frozen_needs_py, is_special_base) =
                    if let Some(inner_type) = inner_type {
                        let is_frozen = PythonBindType::FROZEN_TYPES.contains(&inner_type);
                        let frozen_needs_py =
                            is_frozen && PythonBindType::FROZEN_NEEDS_PY.contains(&inner_type);
                        let is_special_base = if inner_type == "Float" {
                            Some(SpecialBase::FloatT)
                        } else if inner_type == "Bool" {
                            Some(SpecialBase::BoolT)
                        } else {
                            None
                        };

                        (is_frozen, frozen_needs_py, is_special_base)
                    } else {
                        (false, false, None)
                    };

                CustomType {
                    name: name.to_string(),
                    raw_type: raw_type.to_string(),
                    rust_type,
                    is_frozen,
                    frozen_needs_py,
                    is_special_base,
                    snake_case_name: String::new(),
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

            let sig_part = match &variable_info.rust_type {
                RustType::Option(_, _) => {
                    if variable_info.is_special_base.is_some() {
                        needs_python = true;
                    }

                    format!("{variable_name}=None")
                }
                RustType::Union(_) => {
                    if !self.is_frozen {
                        needs_python = true;
                    }

                    format!("{variable_name}=None")
                }
                RustType::Box(_) | RustType::Custom(_) => {
                    if self.is_frozen {
                        format!("{variable_name}=Default::default()")
                    } else {
                        needs_python = !self.is_frozen;
                        format!("{variable_name}=None")
                    }
                }
                RustType::Vec(InnerVecType::U8) => {
                    needs_python = true;
                    format!("{variable_name}=None")
                }
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

        write_fmt!(
            self,
            "    #[pyo3(signature = ({}))]",
            signature_parts.join(", ")
        );
        write_str!(self, "    pub fn new(");

        if needs_python {
            write_str!(self, "        py: Python,");
        }

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            let variable_type = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::Custom(inner_type)) => Cow::Owned(if self.is_frozen {
                    format!("Vec<super::{inner_type}>")
                } else {
                    format!("Vec<Py<super::{inner_type}>>")
                }),
                RustType::Vec(InnerVecType::Base(inner_type)) => {
                    Cow::Owned(format!("Vec<{}>", inner_type))
                }
                RustType::Vec(InnerVecType::String) => Cow::Borrowed("Vec<String>"),
                RustType::Vec(InnerVecType::U8) => Cow::Borrowed("Option<Py<PyBytes>>"),
                RustType::Box(inner_type) => Cow::Owned(if self.is_frozen {
                    format!("super::{inner_type}")
                } else {
                    format!("Option<Py<super::{inner_type}>>")
                }),
                RustType::Option(InnerOptionType::BaseType, inner_type)
                | RustType::Option(InnerOptionType::String, inner_type) => {
                    Cow::Owned(format!("Option<{inner_type}>"))
                }
                RustType::Option(_, inner_type) => Cow::Owned(if inner_type == "Float" {
                    String::from("Option<crate::Floats>")
                } else if inner_type == "Bool" {
                    String::from("Option<crate::Bools>")
                } else if self.is_frozen {
                    format!("Option<super::{inner_type}>")
                } else {
                    format!("Option<Py<super::{inner_type}>>")
                }),
                RustType::Base(inner_type) => Cow::Borrowed(inner_type.as_str()),
                RustType::String => Cow::Borrowed("String"),
                RustType::Union(inner_type) => {
                    Cow::Owned(format!("Option<super::{inner_type}Union>"))
                }
                RustType::Custom(inner_type) => Cow::Owned(if self.is_frozen {
                    format!("super::{inner_type}")
                } else {
                    format!("Option<Py<super::{inner_type}>>")
                }),
                RustType::Other(inner_type) => Cow::Owned(format!("super::{inner_type}")),
            };

            write_fmt!(self, "        {variable_name}: {variable_type},");
        }

        write_str!(self, "    ) -> Self {");
        write_str!(self, "        Self {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.is_special_base.is_some() {
                write_fmt!(
                    self,
                    "            {variable_name}: {variable_name}.map(|x| x.into_gil(py)),"
                );
                continue;
            }

            match &variable_info.rust_type {
                RustType::Union(inner_type) => {
                    if self.is_frozen {
                        write_fmt!(
                            self,
                            "            {variable_name}: super::{}::new({variable_name}),",
                            inner_type
                        );
                    } else {
                        write_fmt!(
                            self,
                            "            {variable_name}: Py::new(py, super::{}::new({variable_name})).unwrap(),",
                            inner_type
                        );
                    }
                }
                RustType::Box(inner_type) | RustType::Custom(inner_type) if !self.is_frozen => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| super::{inner_type}::py_default(py)),",
                    );
                }
                RustType::Vec(InnerVecType::U8) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: {variable_name}.unwrap_or_else(|| PyBytes::new(py, &[]).unbind()),"
                    );
                }
                _ => write_fmt!(self, "            {variable_name},"),
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
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

        if !self.frozen_needs_py {
            write_str!(self, "    #[allow(unused_variables)]");
        }

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
        write_fmt!(
            self,
            "            \"{}({repr_signature})\",",
            self.struct_name
        );

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
                        InnerVecType::Custom(_) => {
                            write_str!(self, "                .iter()");
                            write_str!(
                                self,
                                if self.is_frozen {
                                    "                .map(|x| x.__repr__(py))"
                                } else {
                                    "                .map(|x| x.borrow(py).__repr__(py))"
                                }
                            );
                        }
                    }
                    write_str!(self, "                .collect::<Vec<String>>()");
                    write_str!(self, "                .join(\", \"),");
                }
                RustType::Option(inner_type, _) => {
                    write_fmt!(self, "            self.{variable_name}");
                    write_str!(self, "                .as_ref()");

                    match inner_type {
                        InnerOptionType::BaseType | InnerOptionType::String => {
                            write_str!(self, "                .map(|i| format!(\"{i:?}\"))");
                        }
                        _ => {
                            write_str!(
                                self,
                                if self.is_frozen {
                                    "                .map(|x| x.__repr__(py))"
                                } else {
                                    "                .map(|x| x.borrow(py).__repr__(py))"
                                }
                            );
                        }
                    }

                    write_str!(self, "                .unwrap_or_else(crate::none_str),");
                }
                RustType::Union(_) => {
                    let repr_str = if self.is_frozen {
                        ".inner_repr(py)"
                    } else {
                        ".borrow(py).inner_repr(py)"
                    };

                    write_fmt!(self, "            self.{variable_name}{repr_str},");
                }
                RustType::Box(_) | RustType::Custom(_) => {
                    let repr_str = if self.is_frozen {
                        ".__repr__(py)"
                    } else {
                        ".borrow(py).__repr__(py)"
                    };

                    write_fmt!(self, "            self.{variable_name}{repr_str},");
                }
                RustType::Base(inner_type) => {
                    if inner_type == "bool" {
                        write_fmt!(
                            self,
                            "            crate::bool_to_str(self.{variable_name}),"
                        );
                    } else {
                        write_fmt!(self, "            self.{variable_name},");
                    }
                }
                RustType::String => {
                    write_fmt!(self, "            self.{variable_name},");
                }
                RustType::Other(_) => {
                    write_fmt!(self, "            self.{variable_name}.__repr__(),");
                }
            }
        }

        write_str!(self, "        )");
        write_str!(self, "    }");
    }

    fn generate_long_match_args(&mut self) {
        write_str!(self, "    #[classattr]");
        write_str!(
            self,
            "    fn __match_args__(py: Python) -> Bound<pyo3::types::PyTuple> {"
        );
        write_str!(self, "        pyo3::types::PyTuple::new(py, [");

        for variable_info in &self.types {
            write_fmt!(self, "            \"{}\",", variable_info.name);
        }

        write_str!(self, "        ]).unwrap()");
        write_str!(self, "    }");
    }

    fn generate_match_args(&mut self) {
        if self.types.is_empty() {
            return;
        }

        if self.types.len() > 12 {
            self.generate_long_match_args();
            return;
        }

        let sig_parts: Vec<_> = repeat("&'static str").take(self.types.len()).collect();
        let sig = sig_parts.join(", ");

        write_str!(self, "    #[classattr]");
        write_fmt!(self, "    fn __match_args__() -> ({sig},) {{",);
        write_str!(self, "        (");

        for variable_info in &self.types {
            write_fmt!(self, "            \"{}\",", variable_info.name);
        }

        write_str!(self, "        )");
        write_str!(self, "    }");
    }

    fn generate_pack_method(&mut self) {
        write_str!(
            self,
            "    fn pack<'py>(&self, py: Python<'py>) -> Bound<'py, PyBytes> {"
        );
        write_fmt!(
            self,
            "        let flat_t = flat::{}::from_gil(py, self);",
            &self.struct_t_name
        );

        if self.has_complex_pack {
            write_str!(
                self,
                "        let size = flat_t.get_size().next_power_of_two();"
            );
            write_str!(self, "");
            write_str!(
                self,
                "        let mut builder = FlatBufferBuilder::with_capacity(size);"
            );
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

    fn generate_unpack_method(&mut self) {
        write_str!(self, "    #[staticmethod]");

        let (py_arg, return_val, out_map) = if self.frozen_needs_py {
            ("py: Python, ", "Self", "flat_t.unpack().into_gil(py)")
        } else if self.is_frozen {
            ("", "Self", "flat_t.unpack().into()")
        } else {
            (
                "py: Python, ",
                "Py<Self>",
                "crate::into_py_from(py, flat_t.unpack())",
            )
        };

        write_fmt!(
            self,
            "    fn unpack({py_arg}data: &[u8]) -> PyResult<{return_val}> {{"
        );
        write_fmt!(
            self,
            "        match root::<flat::{}>(data) {{",
            self.struct_name
        );
        write_fmt!(self, "            Ok(flat_t) => Ok({out_map}),");
        write_str!(self, "            Err(e) => Err(flat_err_to_py(e)),");
        write_str!(self, "        }");
        write_str!(self, "    }");
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

    fn add_get_size_derive(&self, path: &Path) {
        let mut contents = fs::read_to_string(path).unwrap();

        #[cfg(windows)]
        {
            contents = contents.replace("\r\n", "\n");
        }

        contents = contents.replace(
            "use self::flatbuffers",
            "use get_size::GetSize;\nuse self::flatbuffers",
        );

        contents = contents.replace(
            "#[derive(Debug, Clone, PartialEq)]\n",
            "#[derive(Debug, Clone, PartialEq, GetSize)]\n",
        );

        contents = contents.replace(
            "#[derive(Debug, Clone, PartialEq, Default)]\n",
            "#[derive(Debug, Clone, PartialEq, Default, GetSize)]\n",
        );

        fs::write(path, contents).unwrap();
    }

    fn generate_definition(&mut self) {
        write_str!(
            self,
            if self.is_frozen {
                "#[pyclass(module = \"rlbot_flatbuffers\", subclass, get_all, frozen)]"
            } else if self.types.is_empty() {
                "#[pyclass(module = \"rlbot_flatbuffers\", subclass, frozen)]"
            } else {
                "#[pyclass(module = \"rlbot_flatbuffers\", subclass, get_all, set_all)]"
            }
        );

        if self.types.is_empty() {
            write_str!(self, "#[derive(Debug, Default, Clone, Copy)]");
            write_fmt!(self, "pub struct {} {{}}", self.struct_name);
            write_str!(self, "");
            return;
        }

        write_str!(
            self,
            if self.is_frozen || self.is_all_base_types {
                if !self.is_all_base_types
                    || self.types.iter().any(|t| t.rust_type == RustType::String)
                {
                    "#[derive(Debug, Default, Clone)]"
                } else {
                    "#[derive(Debug, Default, Clone, Copy)]"
                }
            } else {
                "#[derive(Debug)]"
            }
        );
        write_fmt!(self, "pub struct {} {{", self.struct_name);

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            let variable_type = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => String::from("Py<PyBytes>"),
                RustType::Vec(InnerVecType::String) => String::from("Vec<String>"),
                RustType::Vec(InnerVecType::Base(inner_type)) => format!("Vec<{}>", inner_type),
                RustType::Vec(InnerVecType::Custom(inner_type)) => {
                    if self.is_frozen {
                        format!("Vec<super::{inner_type}>")
                    } else {
                        format!("Vec<Py<super::{inner_type}>>")
                    }
                }
                RustType::Box(inner_type) => {
                    if self.is_frozen {
                        format!("super::{inner_type}")
                    } else {
                        format!("Py<super::{inner_type}>")
                    }
                }
                RustType::Option(InnerOptionType::BaseType, inner_type)
                | RustType::Option(InnerOptionType::String, inner_type) => {
                    format!("Option<{inner_type}>")
                }
                RustType::Option(_, inner_type) => {
                    if self.is_frozen {
                        format!("Option<super::{inner_type}>")
                    } else {
                        format!("Option<Py<super::{inner_type}>>")
                    }
                }
                RustType::Base(inner_type) => inner_type.clone(),
                RustType::String => String::from("String"),
                RustType::Union(inner_type) | RustType::Custom(inner_type) => {
                    if self.is_frozen {
                        format!("super::{inner_type}")
                    } else {
                        format!("Py<super::{inner_type}>")
                    }
                }
                RustType::Other(inner_type) => format!("super::{inner_type}"),
            };

            write_fmt!(self, "    pub {variable_name}: {variable_type},");
        }

        write_str!(self, "}");
        write_str!(self, "");

        if self.is_all_base_types {
            return;
        }

        if self.is_frozen {
            return;
        }

        write_fmt!(self, "impl crate::PyDefault for {} {{", self.struct_name);
        write_str!(self, "    fn py_default(py: Python) -> Py<Self> {");
        write_str!(self, "        Py::new(py, Self {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            let end = match &variable_info.rust_type {
                RustType::Vec(InnerVecType::U8) => Cow::Borrowed("PyBytes::new(py, &[]).unbind()"),
                RustType::Vec(_) => Cow::Borrowed("Vec::new()"),
                RustType::Option(_, _) => Cow::Borrowed("None"),
                RustType::Union(inner_type)
                | RustType::Box(inner_type)
                | RustType::Custom(inner_type) => {
                    Cow::Owned(format!("super::{inner_type}::py_default(py)"))
                }
                RustType::String | RustType::Base(_) | RustType::Other(_) => {
                    Cow::Borrowed("Default::default()")
                }
            };

            write_fmt!(self, "            {variable_name}: {end},");
        }

        if self.is_frozen {
            write_str!(self, "        }");
        } else {
            write_str!(self, "        }).unwrap()");
        }

        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");
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

        let (is_simple, trait_name, fn_name, python_arg) =
            if (self.is_frozen && !self.frozen_needs_py) || self.is_all_base_types {
                (true, "From", "from", "")
            } else {
                (false, "FromGil", "from_gil", "py: Python, ")
            };

        write_fmt!(
            self,
            "impl {trait_name}<{impl_type}> for {} {{",
            self.struct_name
        );

        if !is_simple {
            write_str!(self, "    #[allow(unused_variables)]")
        }

        write_fmt!(
            self,
            "    fn {fn_name}({python_arg}flat_t: {impl_type}) -> Self {{"
        );
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
                RustType::Vec(InnerVecType::Custom(_)) => {
                    if self.is_frozen {
                        let map_out = if variable_info.frozen_needs_py {
                            "|x| x.into_gil(py)"
                        } else {
                            "Into::into"
                        };

                        write_fmt!(
                            self,
                            "            {variable_name}: flat_t.{variable_name}.into_iter().map({map_out}).collect(),"
                        )
                    } else {
                        write_fmt!(
                            self,
                            "            {variable_name}: flat_t.{variable_name}.into_iter().map(|x| crate::into_py_from(py, x)).collect(),",
                        )
                    }
                }
                RustType::Option(InnerOptionType::Box, _) => {
                    let inner = if self.is_frozen {
                        "(*x).into()"
                    } else {
                        "crate::into_py_from(py, *x)"
                    };

                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.map(|x| {inner}),"
                    );
                }
                RustType::Option(InnerOptionType::String, _) => {
                    write_fmt!(self, "            {variable_name}: flat_t.{variable_name},");
                }
                RustType::Option(_, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.map(|x| crate::into_py_from(py, x)),"
                    );
                }
                RustType::Box(_) => {
                    let end = if self.is_frozen {
                        if variable_info.frozen_needs_py {
                            format!("(*flat_t.{variable_name}).into_gil(py)",)
                        } else {
                            format!("(*flat_t.{variable_name}).into()",)
                        }
                    } else {
                        format!("crate::into_py_from(py, *flat_t.{variable_name})")
                    };
                    write_fmt!(self, "            {variable_name}: {end},",);
                }
                RustType::Union(_) | RustType::Custom(_) => {
                    let end = if self.is_frozen {
                        if variable_info.frozen_needs_py {
                            format!("flat_t.{variable_name}.into_gil(py)")
                        } else {
                            format!("flat_t.{variable_name}.into()")
                        }
                    } else {
                        format!("crate::into_py_from(py, flat_t.{variable_name})")
                    };

                    write_fmt!(self, "            {variable_name}: {end},",);
                }
                RustType::Base(_) | RustType::String => {
                    write_fmt!(self, "            {variable_name}: flat_t.{variable_name},");
                }
                RustType::Other(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: flat_t.{variable_name}.into(),",
                    );
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

        let (is_simple, trait_name, fn_name, python_arg) =
            if (self.is_frozen && !self.frozen_needs_py) || self.is_all_base_types {
                (true, "From", "from", "")
            } else {
                (false, "FromGil", "from_gil", "py: Python, ")
            };

        write_fmt!(
            self,
            "impl {trait_name}<&{}> for {impl_type} {{",
            self.struct_name
        );

        if !is_simple {
            write_str!(self, "    #[allow(unused_variables)]")
        }

        write_fmt!(
            self,
            "    fn {fn_name}({python_arg}py_type: &{}) -> Self {{",
            self.struct_name
        );
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
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.clone(),"
                    )
                }
                RustType::Vec(InnerVecType::Custom(_)) => {
                    if self.is_frozen {
                        let map_out = if variable_info.frozen_needs_py {
                            "|x| x.into_gil(py)"
                        } else {
                            "Into::into"
                        };

                        write_fmt!(
                            self,
                            "            {variable_name}: py_type.{variable_name}.iter().map({map_out}).collect(),"
                        )
                    } else {
                        write_fmt!(
                            self,
                            "            {variable_name}: py_type.{variable_name}.iter().map(|x| crate::from_py_into(py, x)).collect(),",
                        )
                    }
                }
                RustType::Option(InnerOptionType::Box, _) => {
                    let inner = if self.is_frozen {
                        "x.into()"
                    } else {
                        "crate::from_py_into(py, x)"
                    };

                    write_fmt!(self, "            {variable_name}: py_type.{variable_name}.as_ref().map(|x| Box::new({inner})),");
                }
                RustType::Option(InnerOptionType::String, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.clone(),"
                    );
                }
                RustType::Option(_, _) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.as_ref().map(|x| crate::from_py_into(py, x)),"
                    );
                }
                RustType::Box(_) => {
                    let var_name = if self.is_frozen {
                        if variable_info.frozen_needs_py {
                            format!("(&py_type.{variable_name}).into_gil(py)")
                        } else {
                            format!("(&py_type.{variable_name}).into()")
                        }
                    } else {
                        format!("crate::from_py_into(py, &py_type.{variable_name})")
                    };

                    write_fmt!(self, "            {variable_name}: Box::new({var_name}),",);
                }
                RustType::Union(_) | RustType::Custom(_) => {
                    let end = if self.is_frozen {
                        if variable_info.frozen_needs_py {
                            format!("(&py_type.{variable_name}).into_gil(py)")
                        } else {
                            format!("(&py_type.{variable_name}).into()")
                        }
                    } else {
                        format!("crate::from_py_into(py, &py_type.{variable_name})")
                    };

                    write_fmt!(self, "            {variable_name}: {end},",);
                }
                RustType::String => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name}.clone(),",
                    );
                }
                RustType::Base(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: py_type.{variable_name},"
                    );
                }
                RustType::Other(_) => {
                    write_fmt!(
                        self,
                        "            {variable_name}: (&py_type.{variable_name}).into(),",
                    );
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

        self.generate_match_args();
        write_str!(self, "");

        self.generate_pack_method();
        write_str!(self, "");

        self.generate_unpack_method();
        write_str!(self, "}");
        write_str!(self, "");
    }
}
