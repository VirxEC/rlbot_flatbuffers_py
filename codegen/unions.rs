use crate::{enums::CustomEnumType, generator::Generator, PythonBindType};
use std::{borrow::Cow, fs, path::Path};

pub struct UnionBindGenerator {
    pub filename: String,
    pub struct_name: String,
    struct_t_name: String,
    pub types: Vec<CustomEnumType>,
    file_contents: Vec<Cow<'static, str>>,
    is_frozen: bool,
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

impl UnionBindGenerator {
    pub fn new(
        filename: String,
        struct_name: String,
        struct_t_name: String,
        types: Vec<CustomEnumType>,
    ) -> Option<Self> {
        let is_frozen = PythonBindType::FROZEN_TYPES.contains(&struct_name.as_str());

        let file_contents = vec![
            Cow::Borrowed("use crate::{generated::rlbot::flat, FromGil};"),
            Cow::Borrowed("use pyo3::{pyclass, pymethods, Bound, Py, PyAny, Python};"),
            Cow::Borrowed(""),
        ];

        Some(Self {
            filename: filename.to_string(),
            struct_name,
            struct_t_name,
            types,
            file_contents,
            is_frozen,
        })
    }

    fn generate_new_method(&mut self) {
        assert!(u8::try_from(self.types.len()).is_ok());

        write_str!(self, "    #[new]");
        write_str!(self, "    #[pyo3(signature = (item = None))]");
        write_fmt!(
            self,
            "    pub fn new(item: Option<{}Union>) -> Self {{",
            self.struct_name
        );
        write_str!(self, "        Self { item }");
        write_str!(self, "    }");
        write_str!(self, "");
        write_str!(self, "    #[getter(item)]");

        write_str!(
            self,
            "    pub fn get(&self, py: Python) -> Option<Py<PyAny>> {"
        );
        write_str!(self, "        match self.item.as_ref() {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_name == "NONE" {
                write_str!(self, "            None => None,");
            } else {
                write_fmt!(
                    self,
                    "            Some({}Union::{variable_name}(item)) => Some(item.clone_ref(py).into_any()),",
                    self.struct_name
                );
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

    fn generate_inner_repr_method(&mut self) {
        write_str!(self, "    pub fn inner_repr(&self, py: Python) -> String {");
        write_str!(self, "        match self.item.as_ref() {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.value.is_some() {
                write_fmt!(
                    self,
                    "            Some({}Union::{variable_name}(item)) => item.borrow(py).__repr__(py),",
                    self.struct_name
                );
            } else {
                write_str!(self, "            None => crate::none_str(),");
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
    }

    fn generate_repr_method(&mut self) {
        write_str!(self, "    pub fn __repr__(&self, py: Python) -> String {");
        write_str!(self, "        match self.item.as_ref() {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.value.is_some() {
                write_fmt!(
                    self,
                    "            Some({}Union::{variable_name}(item)) => format!(\"{}({{}})\", item.borrow(py).__repr__(py)),",
                    self.struct_name,
                    self.struct_name
                );
            } else {
                write_fmt!(
                    self,
                    "            None => String::from(\"{}()\"),",
                    self.struct_name
                );
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
    }
}

impl Generator for UnionBindGenerator {
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

        fs::write(path, contents).unwrap();
    }

    fn generate_definition(&mut self) {
        write_fmt!(self, "#[derive(Debug, pyo3::FromPyObject)]");
        write_fmt!(self, "pub enum {}Union {{", self.struct_name);

        for variable_info in self.types.iter().skip(1) {
            let variable_name = variable_info.name.as_str();

            write_fmt!(self, "    {variable_name}(Py<super::{variable_name}>),");
        }

        write_str!(self, "}");
        write_str!(self, "");

        if self.is_frozen {
            write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\", frozen)]");
        } else {
            write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\")]");
        }

        write_fmt!(self, "#[derive(Debug, Default)]");
        write_fmt!(self, "pub struct {} {{", self.struct_name);

        if !self.is_frozen {
            write_str!(self, "    #[pyo3(set)]");
        }

        write_fmt!(self, "    pub item: Option<{}Union>,", self.struct_name);
        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_from_flat_impls(&mut self) {
        write_fmt!(
            self,
            "impl FromGil<flat::{}> for {} {{",
            self.struct_t_name,
            self.struct_name
        );
        write_fmt!(
            self,
            "    fn from_gil(py: Python, flat_t: flat::{}) -> Self {{",
            self.struct_t_name
        );

        write_str!(self, "        match flat_t {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_name == "NONE" {
                write_fmt!(
                    self,
                    "            flat::{}::NONE => {}::default(),",
                    self.struct_t_name,
                    self.struct_name
                );
            } else {
                write_fmt!(
                    self,
                    "            flat::{}::{variable_name}(item) => {} {{",
                    self.struct_t_name,
                    self.struct_name,
                );

                write_fmt!(
                    self,
                    "                item: Some({}Union::{variable_name}(",
                    self.struct_name
                );

                write_fmt!(
                    self,
                    "                    Py::new(py, super::{variable_name}::from_gil(py, *item)).unwrap(),"
                );

                write_fmt!(self, "                )),");
                write_fmt!(self, "            }},");
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_to_flat_impls(&mut self) {
        write_fmt!(
            self,
            "impl FromGil<&{}> for flat::{} {{",
            self.struct_name,
            self.struct_t_name
        );
        write_fmt!(
            self,
            "    fn from_gil(py: Python, py_type: &{}) -> Self {{",
            self.struct_name
        );

        write_str!(self, "        match py_type.item.as_ref() {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if let Some(ref value) = variable_info.value {
                write_fmt!(
                    self,
                    "            Some({}Union::{value}(item)) => {{",
                    self.struct_name,
                );

                write_fmt!(
                    self,
                    "                flat::{}::{variable_name}(Box::new(crate::from_py_into(py, item)))",
                    self.struct_t_name
                );

                write_str!(self, "            },");
            } else {
                write_str!(self, "            None => Self::NONE,");
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

        self.generate_inner_repr_method();
        write_str!(self, "");

        self.generate_repr_method();
        write_str!(self, "}");
        write_str!(self, "");
    }
}
