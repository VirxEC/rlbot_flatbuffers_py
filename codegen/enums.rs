use crate::generator::Generator;
use std::{borrow::Cow, fs, path::Path};

pub struct CustomEnumType {
    pub name: String,
    pub raw_type: String,
    pub value: Option<String>,
    pub snake_case_name: String,
    pub doc_str: Option<Vec<String>>,
}

fn camel_to_snake_case(variable_name: &str) -> String {
    let mut snake_case_parts: Vec<String> = Vec::new();

    let mut last_was_uppercase = false;
    for c in variable_name.chars() {
        if c.is_uppercase() {
            if last_was_uppercase {
                snake_case_parts.last_mut().unwrap().push(c.to_lowercase().next().unwrap());
            } else {
                snake_case_parts.push(c.to_lowercase().to_string());
            }

            last_was_uppercase = true;
        } else if c.is_ascii_digit() {
            snake_case_parts.push(c.to_lowercase().to_string());
            last_was_uppercase = false;
        } else {
            snake_case_parts.last_mut().unwrap().push(c);
            last_was_uppercase = false;
        }
    }

    snake_case_parts.join("_")
}

pub enum EnumType {
    Enum,
    Union,
}

pub struct EnumBindGenerator {
    pub filename: String,
    pub struct_name: String,
    pub types: Vec<CustomEnumType>,
    file_contents: Vec<Cow<'static, str>>,
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

impl EnumBindGenerator {
    pub fn new(filename: String, struct_name: String, types: Vec<CustomEnumType>) -> Option<Self> {
        let file_contents = vec![
            Cow::Borrowed("use crate::{flat_err_to_py, generated::rlbot::flat};"),
            Cow::Borrowed("use flatbuffers::root;"),
            Cow::Borrowed(
                "use pyo3::{exceptions::PyValueError, pyclass, pymethods, types::PyBytes, Bound, PyResult, Python};",
            ),
            Cow::Borrowed(""),
        ];

        Some(Self {
            filename: filename.to_string(),
            struct_name,
            types,
            file_contents,
        })
    }

    pub fn raw_types_to_custom(raw_types: Vec<(&str, &str, Option<Vec<String>>)>) -> Vec<CustomEnumType> {
        raw_types
            .into_iter()
            .map(|(name, raw_type, doc_str)| CustomEnumType {
                name: name.to_string(),
                raw_type: raw_type.to_string(),
                value: None,
                snake_case_name: String::new(),
                doc_str,
            })
            .collect()
    }

    pub fn get_types(contents: &str, struct_name: &str) -> Option<(Vec<CustomEnumType>, EnumType)> {
        let struct_definition = format!("pub struct {struct_name}(pub u8);\n");
        let struct_pos = contents.find(&struct_definition)?;

        // find 'impl CollisionShape {\n'
        let impl_definition = format!("impl {struct_name} {{\n");
        let impl_pos = contents[struct_pos..].find(&impl_definition)?;

        // the next lines should be in the format 'pub const {some value}: Self = Self(i);\n'
        let mut file_line_start = struct_pos + impl_pos + impl_definition.len();
        let mut lines = contents[file_line_start..].split('\n');
        let mut types = Vec::new();
        let mut docs = Vec::new();

        loop {
            let line = lines.next()?;
            let line_trim = line.trim();

            if line_trim.is_empty() {
                break;
            }

            if line_trim.starts_with("///") {
                docs.push(line_trim.trim_start_matches("///").trim().to_string());
                file_line_start += line.len();
                continue;
            }

            if line_trim.starts_with("//") {
                file_line_start += line.len();
                docs.clear();
                continue;
            }

            let definition = line_trim.trim_start_matches("pub const ").trim_end_matches(';');

            let mut parts = definition.split(": Self = ");

            let variable_name = parts.next()?;
            let variable_value = parts.next()?.trim_start_matches("Self(").trim_end_matches(')');

            let docs = if docs.is_empty() {
                None
            } else {
                let docs_copy = docs.clone();
                docs.clear();

                Some(docs_copy)
            };

            types.push((variable_name, variable_value, docs));

            file_line_start += line.len();
        }

        if types.is_empty() {
            return None;
        }

        let mut custom_types = Self::raw_types_to_custom(types);

        let union_definition = format!("pub enum {struct_name}T {{\n");
        let Some(union_start) = contents.find(&union_definition) else {
            return Some((custom_types, EnumType::Enum));
        };

        let union_end_definition = "}\n";
        let union_end = contents[union_start..].find(union_end_definition).unwrap();

        let union_definition =
            &contents[union_start + union_definition.len()..union_start + union_end - union_end_definition.len()];

        for (line, variable) in union_definition.split('\n').zip(&mut custom_types) {
            let line_trim = line.trim().trim_start_matches(&variable.name).trim_end_matches(',');

            if line_trim.is_empty() {
                variable.value = None;
                continue;
            }

            variable.snake_case_name = camel_to_snake_case(variable.name.as_str());

            let new_type = line_trim.trim_start_matches("(Box<").trim_end_matches("T>)");
            variable.value = Some(new_type.to_string());
        }

        Some((custom_types, EnumType::Union))
    }

    fn generate_new_method(&mut self) {
        write_str!(self, "    #[new]");
        assert!(u8::try_from(self.types.len()).is_ok());

        write_str!(self, "    #[pyo3(signature = (value=Default::default()))]");
        write_str!(self, "    pub fn new(value: u8) -> PyResult<Self> {");
        write_str!(self, "        match value {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();
            write_fmt!(self, "            {} => Ok(Self::{variable_name}),", variable_info.raw_type);
        }

        if self.types.len() != usize::from(u8::MAX) {
            write_str!(
                self,
                "            v => Err(PyValueError::new_err(format!(\"Unknown value of {v}\"))),"
            );
        }

        write_str!(self, "        }");
        write_str!(self, "    }");
    }

    fn generate_str_method(&mut self) {
        write_str!(self, "    pub fn __str__(&self) -> String {");
        write_str!(self, "        self.__repr__()");
        write_str!(self, "    }");
    }

    fn generate_repr_method(&mut self) {
        write_str!(self, "    pub fn __repr__(&self) -> String {");
        write_fmt!(self, "        format!(\"{}.{{self:?}}\")", self.struct_name);
        write_str!(self, "    }");
    }
}

impl Generator for EnumBindGenerator {
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

        contents = contents.replace(
            "#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]\n",
            "#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, GetSize)]\n",
        );

        let start = contents.find("impl core::fmt::Debug for ").unwrap();
        let end = contents[start..].find("\n}").unwrap() + 3;
        contents.replace_range(start..start + end, "");

        let start = contents.find("  pub fn variant_name").unwrap();
        let prev_line_start = contents[..start].rfind(";\n").unwrap();
        let end = contents[start..].find("  }\n").unwrap() + 7;
        contents.replace_range(prev_line_start + 1..start + end, "");

        for val in ["ENUM_MIN", "ENUM_MAX", "ENUM_VALUES"] {
            let line_start = contents.find(&format!("\npub const {val}")).unwrap();
            let prev_line_start = contents[..line_start].rfind(";\n").unwrap();
            let line_end = contents[line_start..].find(";\n").unwrap();

            contents.replace_range(prev_line_start..line_start + line_end, "");
        }

        fs::write(path, contents).unwrap();
    }

    fn generate_definition(&mut self) {
        write_str!(self, "#[allow(non_camel_case_types)]");
        write_str!(self, "#[pyclass(module = \"rlbot_flatbuffers\", frozen, hash, eq, eq_int)]");
        write_str!(self, "#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]");
        write_fmt!(self, "pub enum {} {{", self.struct_name);
        write_str!(self, "    #[default]");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();
            write_fmt!(self, "    {variable_name} = {},", variable_info.raw_type);
        }

        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_from_flat_impls(&mut self) {
        write_fmt!(self, "impl From<flat::{}> for {} {{", self.struct_name, self.struct_name);
        write_fmt!(self, "    fn from(flat_t: flat::{}) -> Self {{", self.struct_name);
        write_str!(self, "        match flat_t {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            write_fmt!(
                self,
                "            flat::{}::{variable_name} => Self::{variable_name},",
                self.struct_name
            );
        }

        write_fmt!(self, "            _ => Self::{},", self.types.last().unwrap().name.as_str());

        write_str!(self, "        }");
        write_str!(self, "    }");
        write_str!(self, "}");
        write_str!(self, "");
    }

    fn generate_to_flat_impls(&mut self) {
        write_fmt!(self, "impl From<&{}> for flat::{} {{", self.struct_name, self.struct_name);
        write_fmt!(self, "    fn from(py_type: &{}) -> Self {{", self.struct_name);
        write_str!(self, "        match *py_type {");

        for variable_info in &self.types {
            let variable_name = variable_info.name.as_str();

            write_fmt!(
                self,
                "            {}::{variable_name} => Self::{variable_name},",
                self.struct_name
            );
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
        write_str!(self, "}");
        write_str!(self, "");
    }
}
