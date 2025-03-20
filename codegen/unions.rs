use crate::{PythonBindType, enums::CustomEnumType, generator::Generator};
use std::{borrow::Cow, fs, path::Path};

pub struct UnionBindGenerator {
    pub filename: String,
    pub struct_name: String,
    struct_t_name: String,
    pub types: Vec<CustomEnumType>,
    file_contents: Vec<Cow<'static, str>>,
    is_frozen: bool,
    is_no_set: bool,
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
    pub fn new(filename: String, struct_name: String, struct_t_name: String, types: Vec<CustomEnumType>) -> Option<Self> {
        let is_frozen = PythonBindType::FROZEN_TYPES.contains(&struct_name.as_str());
        let is_no_set = PythonBindType::NO_SET_TYPES.contains(&struct_name.as_str());

        let file_contents = vec![
            Cow::Borrowed("use crate::{generated::rlbot::flat, FromGil, UnpackFrom};"),
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
            is_no_set,
        })
    }

    fn generate_new_method(&mut self) {
        assert!(u8::try_from(self.types.len()).is_ok());

        write_str!(self, "    #[new]");
        write_fmt!(self, "    pub fn new(item: {}Union) -> Self {{", self.struct_name);
        write_str!(self, "        Self { item }");
        write_str!(self, "    }");
        write_str!(self, "");
        write_str!(self, "    #[getter(item)]");

        write_str!(self, "    pub fn get(&self, py: Python) -> Option<Py<PyAny>> {");
        write_str!(self, "        match &self.item {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_name != "NONE" {
                write_fmt!(
                    self,
                    "            {}Union::{variable_name}(item) => Some(item.clone_ref(py).into_any()),",
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
        write_str!(self, "        match &self.item {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.value.is_some() {
                write_fmt!(
                    self,
                    "            {}Union::{variable_name}(item) => item.borrow(py).__repr__(py),",
                    self.struct_name
                );
            }
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
    }

    fn generate_repr_method(&mut self) {
        write_str!(self, "    pub fn __repr__(&self, py: Python) -> String {");
        write_str!(self, "        match &self.item {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_info.value.is_some() {
                write_fmt!(
                    self,
                    "            {}Union::{variable_name}(item) => format!(\"{}({{}})\", item.borrow(py).__repr__(py)),",
                    self.struct_name,
                    self.struct_name
                );
            }
        }

        write_str!(self, "        }");
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
        write_str!(self, "        match flat_t {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_name == "NONE" {
                write_fmt!(self, "            flat::{}::NONE => unreachable!(),", self.struct_t_name);
                continue;
            }

            write_fmt!(
                self,
                "            flat::{}::{variable_name}(flat_item) => {{",
                self.struct_t_name
            );
            write_fmt!(
                self,
                "                if let {}Union::{variable_name}(item) = &self.item {{",
                self.struct_name
            );
            write_fmt!(
                self,
                "                    item.bind_borrowed(py).borrow_mut().unpack_from(py, *flat_item);"
            );
            write_str!(self, "                } else {");
            write_fmt!(
                self,
                "                    self.item = {}Union::{variable_name}(",
                self.struct_name
            );
            write_fmt!(
                self,
                "                        Py::new(py, super::{variable_name}::from_gil(py, *flat_item)).unwrap()"
            );
            write_str!(self, "                    );");
            write_str!(self, "                }");
            write_str!(self, "            },");
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
        write_str!(self, "}");
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

    fn modify_source(&self, path: &Path) {
        let mut contents = fs::read_to_string(path).unwrap();

        #[cfg(windows)]
        {
            contents = contents.replace("\r\n", "\n");
        }

        contents = contents.replace("use self::flatbuffers", "use get_size::GetSize;\nuse self::flatbuffers");

        contents = contents.replace("#[derive(Debug, Clone, PartialEq)]\n", "#[derive(PartialEq, GetSize)]\n");

        // delete unneeded auto-generated source code from flatc
        let start = contents.find("impl core::fmt::Debug for ").unwrap();
        let end = contents[start..].find("\n}").unwrap() + 2;
        contents.replace_range(start..start + end, "");

        for _ in 0..3 {
            let start = contents.find("#[deprecated").unwrap();
            let end = contents[start..].find(";\n").unwrap() + 2;
            contents.replace_range(start..start + end, "");
        }

        let start = contents.find("  /// Returns the variant's name").unwrap();
        let end = contents[start..].find("\n  }\n").unwrap() + 5;
        contents.replace_range(start..start + end, "");

        fs::write(path, contents).unwrap();
    }

    fn generate_definition(&mut self) {
        write_fmt!(self, "#[derive(pyo3::FromPyObject)]");
        write_fmt!(self, "pub enum {}Union {{", self.struct_name);

        for variable_info in self.types.iter().skip(1) {
            let variable_name = variable_info.name.as_str();

            write_fmt!(self, "    {variable_name}(Py<super::{variable_name}>),");
        }

        write_str!(self, "}");
        write_str!(self, "");

        if self.is_frozen {
            write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\", frozen)]");
        } else if self.is_no_set {
            write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\")]");
        } else {
            write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\", set_all)]");
        }

        write_fmt!(self, "pub struct {} {{", self.struct_name);
        write_fmt!(self, "    item: {}Union,", self.struct_name);
        write_str!(self, "}");
        write_str!(self, "");

        write_fmt!(self, "impl crate::PyDefault for {} {{", self.struct_name);
        write_str!(self, "    fn py_default(py: Python) -> Py<Self> {");
        write_str!(self, "        Py::new(py, Self {");
        write_fmt!(
            self,
            "            item: {}Union::{}(super::{}::py_default(py)),",
            self.struct_name,
            self.types[1].name,
            self.types[1].name
        );
        write_str!(self, "        }).unwrap()");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");

        if !self.is_frozen {
            self.generate_unpack_from();
        }
    }

    fn generate_from_flat_impls(&mut self) {
        write_fmt!(self, "impl FromGil<flat::{}> for {} {{", self.struct_t_name, self.struct_name);
        write_fmt!(
            self,
            "    fn from_gil(py: Python, flat_t: flat::{}) -> Self {{",
            self.struct_t_name
        );

        write_str!(self, "        match flat_t {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if variable_name == "NONE" {
                write_fmt!(self, "            flat::{}::NONE => unreachable!(),", self.struct_t_name,);
            } else {
                write_fmt!(
                    self,
                    "            flat::{}::{variable_name}(item) => {} {{",
                    self.struct_t_name,
                    self.struct_name,
                );

                write_fmt!(self, "                item: {}Union::{variable_name}(", self.struct_name);

                write_fmt!(
                    self,
                    "                    Py::new(py, super::{variable_name}::from_gil(py, *item)).unwrap(),"
                );

                write_fmt!(self, "                ),");
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
        write_fmt!(self, "    fn from_gil(py: Python, py_type: &{}) -> Self {{", self.struct_name);

        write_str!(self, "        match &py_type.item {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            if let Some(ref value) = variable_info.value {
                write_fmt!(self, "            {}Union::{value}(item) => {{", self.struct_name,);

                write_fmt!(
                    self,
                    "                flat::{}::{variable_name}(Box::new(crate::from_py_into(py, item)))",
                    self.struct_t_name
                );

                write_str!(self, "            },");
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
