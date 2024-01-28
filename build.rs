use std::{borrow::Cow, env::set_current_dir, fs, io, path::Path, process::Command, string};

const FLATC_BINARY: &str = if cfg!(windows) { "flatc.exe" } else { "flatc" };
const OUT_FOLDER: &str = "./src/generated";
const SCHEMA_FOLDER: &str = "./flatbuffers-schema";
const SCHEMA_FOLDER_BACKUP: &str = "../flatbuffers-schema";

const PYTHON_OUT_FOLDER: &str = "./src/python";

#[derive(Debug, PartialEq, Eq)]
enum PythonBindType {
    Struct,
    Enum,
    Union,
}

struct PythonBindGenerator {
    filename: String,
    struct_name: String,
    struct_t_name: String,
    types: Vec<Vec<String>>,
    file_contents: Vec<Cow<'static, str>>,
    bind_type: PythonBindType,
    has_complex_pack: bool,
}

impl PythonBindGenerator {
    const BASE_TYPES: [&'static str; 5] = ["bool", "i32", "f32", "String", "u8"];

    fn new(path: &Path) -> Option<Self> {
        // get the filename without the extension
        let filename = path.file_stem().unwrap().to_str().unwrap();

        if filename == "mod" {
            return None;
        }

        // convert snake_case to CamelCase to get the struct name
        let mut struct_name = String::new();
        for c in filename.split('_') {
            struct_name.push_str(&c[..1].to_uppercase());
            struct_name.push_str(&c[1..]);
        }
        struct_name = struct_name
            .replace("Rlbot", "RLBot")
            .replace("Halign", "HAlign")
            .replace("Valign", "VAlign");

        let struct_t_name = format!("{struct_name}T");

        let contents = fs::read_to_string(path).ok()?;

        #[cfg(windows)]
        let contents = contents.replace("\r\n", "\n");

        let Some((types, bind_type)) = Self::get_normal_struct_types(&contents, &struct_t_name)
            .or_else(|| Self::get_enum_struct_types(&contents, &struct_name))
        else {
            println!("Could not find struct definition in {filename} for {struct_t_name}");
            return None;
        };

        let has_complex_pack = contents.contains("pub fn pack<'b, A: flatbuffers::Allocator + 'b>(");

        let mut file_contents = vec![Cow::Borrowed("use crate::generated::rlbot::flat;")];

        if bind_type != PythonBindType::Union {
            if has_complex_pack {
                file_contents.push(Cow::Borrowed("use flatbuffers::{root, FlatBufferBuilder};"));
            } else {
                file_contents.push(Cow::Borrowed("use flatbuffers::root;"));
            }
        }

        file_contents.push(Cow::Borrowed("use get_size::GetSize;"));

        if bind_type == PythonBindType::Union {
            file_contents.push(Cow::Borrowed("use pyo3::{pyclass, pymethods};"));
        } else {
            file_contents.push(Cow::Borrowed("use pyo3::{pyclass, pymethods, types::PyBytes, Python};"));
        }

        file_contents.push(Cow::Borrowed(""));

        Some(Self {
            filename: filename.to_string(),
            struct_name,
            struct_t_name,
            types,
            file_contents,
            bind_type,
            has_complex_pack,
        })
    }

    fn get_enum_struct_types(contents: &str, struct_name: &str) -> Option<(Vec<Vec<String>>, PythonBindType)> {
        let struct_definition = format!("pub struct {struct_name}(pub u8);\n");
        let struct_pos = contents.find(&struct_definition)?;

        // find 'impl CollisionShape {\n'
        let impl_definition = format!("impl {struct_name} {{\n");
        let impl_pos = contents[struct_pos..].find(&impl_definition)?;

        // the next lines should be in the format 'pub const {some value}: Self = Self(i);\n'
        let mut file_line_start = struct_pos + impl_pos + impl_definition.len();
        let mut lines = contents[file_line_start..].split('\n');
        let mut types = Vec::new();

        loop {
            let line = lines.next()?;
            let line_trim = line.trim();

            if line_trim.is_empty() {
                break;
            }

            if line_trim.starts_with("//") {
                file_line_start += line.len();
                continue;
            }

            let definition = line_trim.trim_start_matches("pub const ").trim_end_matches(';');

            let mut parts = definition.split(": Self = ");

            let variable_name = parts.next()?;
            let variable_value = parts.next()?.trim_start_matches("Self(").trim_end_matches(')');

            types.push(vec![variable_name.to_string(), variable_value.to_string()]);

            file_line_start += line.len();
        }

        if types.is_empty() {
            return None;
        }

        let union_definition = format!("pub enum {struct_name}T {{\n");
        let Some(union_start) = contents.find(&union_definition) else {
            return Some((types, PythonBindType::Enum));
        };

        let union_end_definition = "}\n";
        let union_end = contents[union_start..].find(union_end_definition).unwrap();

        let union_definition =
            &contents[union_start + union_definition.len()..union_start + union_end - union_end_definition.len()];

        for (line, variables) in union_definition.split('\n').zip(&mut types) {
            let variable_name = variables[0].as_str();

            let line_trim = line.trim().trim_start_matches(variable_name).trim_end_matches(',');

            if line_trim.is_empty() {
                variables[1].clear();
                continue;
            }

            variables.push(camel_to_snake_case(variable_name));

            let new_type = line_trim.trim_start_matches("(Box<").trim_end_matches("T>)");
            variables[1] = new_type.to_string();
        }

        Some((types, PythonBindType::Union))
    }

    fn get_normal_struct_types(contents: &str, struct_t_name: &str) -> Option<(Vec<Vec<String>>, PythonBindType)> {
        // find the struct definition
        let struct_start_definition = format!("pub struct {struct_t_name} {{\n");
        let struct_start = contents.find(&struct_start_definition)?;

        let struct_end_definition = "}\n";
        let struct_end = contents[struct_start..].find(struct_end_definition).unwrap();

        let start = struct_start + struct_start_definition.len();
        let end = struct_start + struct_end - struct_end_definition.len();

        if end <= start {
            return Some((Vec::new(), PythonBindType::Struct));
        }

        let struct_definition = &contents[start..end];

        Some((
            struct_definition
                .split('\n')
                .map(|s| {
                    s.trim_start_matches(' ')
                        .trim_start_matches("pub ")
                        .trim_end_matches(',')
                        .split(": ")
                        .map(string::ToString::to_string)
                        .collect()
                })
                .collect(),
            PythonBindType::Struct,
        ))
    }

    fn write_str(&mut self, s: &'static str) {
        self.file_contents.push(Cow::Borrowed(s));
    }

    fn write_string(&mut self, s: String) {
        self.file_contents.push(Cow::Owned(s));
    }

    fn generate_struct_definition(&mut self) {
        self.write_str("#[pyclass(module = \"rlbot_flatbuffers\", get_all, set_all)]");
        self.write_str("#[derive(Debug, Default, Clone, GetSize)]");
        self.write_string(format!("pub struct {} {{", self.struct_name));

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let mut variable_type = variable_info[1].to_string();

            if variable_type.starts_with("Vec<") && variable_type.ends_with("T>") {
                variable_type = format!(
                    "Vec<super::{}>",
                    variable_type.trim_start_matches("Vec<").trim_end_matches("T>")
                );
            } else if variable_type.starts_with("Box<") && variable_type.ends_with('>') {
                variable_type = format!(
                    "super::{}",
                    variable_type
                        .trim_start_matches("Box<")
                        .trim_end_matches('>')
                        .trim_end_matches('T')
                );
            } else if variable_type.starts_with("Option<") && variable_type.ends_with('>') {
                let inner_type = variable_type
                    .trim_start_matches("Option<")
                    .trim_start_matches("Box<")
                    .trim_end_matches('>')
                    .trim_end_matches('T');

                if Self::BASE_TYPES.contains(&inner_type) {
                    variable_type = format!("Option<{inner_type}>");
                } else {
                    variable_type = format!("Option<super::{inner_type}>");
                }
            } else if variable_type.ends_with('T') {
                variable_type = format!("super::{}", variable_type.trim_end_matches('T'));
            } else if !Self::BASE_TYPES.contains(&variable_type.as_str()) {
                variable_type = format!("super::{variable_type}");
            }

            self.file_contents
                .push(Cow::Owned(format!("    pub {variable_name}: {variable_type},",)));
        }

        self.write_str("}");
        self.write_str("");
    }

    fn generate_enum_definition(&mut self) {
        self.write_str("#[pyclass(module = \"rlbot_flatbuffers\", get_all, set_all)]");
        self.write_str("#[derive(Debug, Default, Clone, Copy, GetSize)]");
        self.write_string(format!("pub enum {} {{", self.struct_name));
        self.write_str("    #[default]");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let variable_value = &variable_info[1];

            self.file_contents
                .push(Cow::Owned(format!("    {variable_name} = {variable_value},",)));
        }

        self.write_str("}");
        self.write_str("");
    }

    fn generate_definition(&mut self) {
        match self.bind_type {
            PythonBindType::Enum => self.generate_enum_definition(),
            PythonBindType::Struct => self.generate_struct_definition(),
            PythonBindType::Union => self.generate_union_definition(),
        }
    }

    fn generate_union_definition(&mut self) {
        self.write_str("#[pyclass(module = \"rlbot_flatbuffers\")]");
        self.write_str("#[derive(Debug, Default, Clone, Copy, GetSize)]");
        self.write_string(format!("pub enum {}Type {{", self.struct_name));
        self.write_str("    #[default]");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            self.file_contents.push(Cow::Owned(format!("    {variable_name},")));
        }

        self.write_str("}");
        self.write_str("");

        self.write_str("#[pyclass(module = \"rlbot_flatbuffers\", get_all, set_all)]");
        self.write_str("#[derive(Debug, Default, Clone, GetSize)]");
        self.write_string(format!("pub struct {} {{", self.struct_name));

        self.file_contents
            .push(Cow::Owned(format!("    pub item_type: {}Type,", self.struct_name)));

        for variable_info in &self.types {
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                continue;
            }

            let snake_case_name = &variable_info[2];

            self.file_contents.push(Cow::Owned(format!(
                "    pub {snake_case_name}: Option<super::{variable_type}>,",
            )));
        }

        self.write_str("}");
        self.write_str("");
    }

    fn generate_from_flat_impls(&mut self) {
        match self.bind_type {
            PythonBindType::Enum => self.generate_enum_from_flat_impls(),
            PythonBindType::Struct => self.generate_struct_from_flat_impls(),
            PythonBindType::Union => self.generate_union_from_flat_impls(),
        }
    }

    fn generate_union_from_flat_impls(&mut self) {
        let from_impl_types = [
            format!("flat::{}", self.struct_t_name),
            format!("&flat::{}", self.struct_t_name),
            format!("Box<flat::{}>", self.struct_t_name),
        ];

        for impl_type in from_impl_types {
            self.write_string(format!("impl From<{impl_type}> for {}Type {{", self.struct_name));
            self.write_string(format!("    fn from(flat_t: {impl_type}) -> Self {{"));

            let is_box = impl_type.starts_with("Box<");
            if is_box {
                self.write_str("        match *flat_t {");
            } else {
                self.write_str("        match flat_t {");
            }

            for variable_info in &self.types {
                let variable_name = &variable_info[0];

                if variable_name == "NONE" {
                    self.file_contents.push(Cow::Owned(format!(
                        "            flat::{}::{variable_name} => Self::NONE,",
                        self.struct_t_name,
                    )));
                } else {
                    self.file_contents.push(Cow::Owned(format!(
                        "            flat::{}::{variable_name}(_) => Self::{variable_name},",
                        self.struct_t_name,
                    )));
                }
            }

            self.write_str("        }");
            self.write_str("    }");
            self.write_str("}");
            self.write_str("");

            if impl_type.starts_with('&') {
                continue;
            }

            self.write_string(format!("impl From<{impl_type}> for {} {{", self.struct_name));
            self.write_string(format!("    fn from(mut flat_t: {impl_type}) -> Self {{"));
            self.write_str("        Self {");
            if is_box {
                self.write_str("            item_type: flat_t.as_ref().into(),");
            } else {
                self.write_str("            item_type: (&flat_t).into(),");
            }

            for variable_info in &self.types {
                if variable_info[1].is_empty() {
                    continue;
                }

                let snake_case_name = &variable_info[2];

                self.file_contents.push(Cow::Owned(format!(
                    "            {snake_case_name}: flat_t.take_{snake_case_name}().map(Into::into),",
                )));
            }

            self.write_str("        }");
            self.write_str("    }");
            self.write_str("}");
            self.write_str("");
        }
    }

    fn generate_enum_from_flat_impls(&mut self) {
        self.write_string(format!("impl From<flat::{}> for {} {{", self.struct_name, self.struct_name));
        self.write_string(format!("    fn from(flat_t: flat::{}) -> Self {{", self.struct_name));
        self.write_str("        match flat_t {");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];

            self.file_contents.push(Cow::Owned(format!(
                "            flat::{}::{variable_name} => Self::{variable_name},",
                self.struct_name
            )));
        }

        self.write_str("            v => unreachable!(\"Unknown value: {v:?}\"),");

        self.write_str("        }");
        self.write_str("    }");
        self.write_str("}");
        self.write_str("");
    }

    fn generate_struct_from_flat_impls(&mut self) {
        let from_impl_types = [
            format!("flat::{}", self.struct_t_name),
            format!("Box<flat::{}>", self.struct_t_name),
        ];

        for impl_type in from_impl_types {
            self.write_string(format!("impl From<{impl_type}> for {} {{", self.struct_name));
            self.write_string(format!("    fn from(flat_t: {impl_type}) -> Self {{"));
            self.write_str("        Self {");

            for variable_info in &self.types {
                let variable_name = &variable_info[0];
                let variable_type = variable_info[1].as_str();

                if variable_type.starts_with("Vec<") {
                    let inner_type = variable_type.trim_start_matches("Vec<").trim_end_matches('>');
                    if Self::BASE_TYPES.contains(&inner_type) {
                        self.file_contents
                            .push(Cow::Owned(format!("            {variable_name}: flat_t.{variable_name},",)));
                    } else {
                        self.file_contents.push(Cow::Owned(format!(
                            "            {variable_name}: flat_t.{variable_name}.into_iter().map(Into::into).collect(),",
                        )));
                    }
                } else if variable_type.starts_with("Option<") {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {variable_name}: flat_t.{variable_name}.map(Into::into),",
                    )));
                } else if Self::BASE_TYPES.contains(&variable_type) {
                    self.file_contents
                        .push(Cow::Owned(format!("            {variable_name}: flat_t.{variable_name},",)));
                } else {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {variable_name}: flat_t.{variable_name}.into(),",
                    )));
                }
            }

            self.write_str("        }");
            self.write_str("    }");
            self.write_str("}");
            self.write_str("");
        }
    }

    fn generate_union_to_flat_impls(&mut self) {
        let from_impl_types = [
            format!("flat::{}", self.struct_t_name),
            format!("Box<flat::{}>", self.struct_t_name),
        ];

        for impl_type in from_impl_types {
            self.write_string(format!("impl From<&{}> for {impl_type} {{", self.struct_name));
            self.write_string(format!("    fn from(py_type: &{}) -> Self {{", self.struct_name));

            let is_box_type = impl_type.contains("Box<");
            if is_box_type {
                self.write_str("        Box::new(match py_type.item_type {");
            } else {
                self.write_str("        match py_type.item_type {");
            }

            for variable_info in &self.types {
                let variable_name = &variable_info[0];
                let variable_value = &variable_info[1];

                if variable_value.is_empty() {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {}Type::NONE => flat::{}::NONE,",
                        self.struct_name, self.struct_t_name,
                    )));
                } else {
                    let snake_case_name = &variable_info[2];

                    self.file_contents.push(Cow::Owned(format!(
                        "            {}Type::{variable_value} => flat::{}::{variable_name}(Box::from(py_type.{snake_case_name}.as_ref().unwrap())),",
                        self.struct_name,
                        self.struct_t_name,
                    )));
                }
            }

            if is_box_type {
                self.write_str("        })");
            } else {
                self.write_str("        }");
            }

            self.write_str("    }");
            self.write_str("}");
            self.write_str("");
        }
    }

    fn generate_to_flat_impls(&mut self) {
        match self.bind_type {
            PythonBindType::Enum => self.generate_enum_to_flat_impls(),
            PythonBindType::Struct => self.generate_struct_to_flat_impls(),
            PythonBindType::Union => self.generate_union_to_flat_impls(),
        }
    }

    fn generate_enum_to_flat_impls(&mut self) {
        self.write_string(format!("impl From<&{}> for flat::{} {{", self.struct_name, self.struct_name));
        self.write_string(format!("    fn from(py_type: &{}) -> Self {{", self.struct_name));
        self.write_str("        match *py_type {");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];

            self.file_contents.push(Cow::Owned(format!(
                "            {}::{variable_name} => Self::{variable_name},",
                self.struct_name
            )));
        }

        self.write_str("        }");
        self.write_str("    }");
        self.write_str("}");

        self.write_str("");
    }

    fn generate_struct_to_flat_impls(&mut self) {
        let from_impl_types = [
            format!("flat::{}", self.struct_t_name),
            format!("Box<flat::{}>", self.struct_t_name),
        ];

        for impl_type in from_impl_types {
            self.write_string(format!("impl From<&{}> for {impl_type} {{", self.struct_name));
            self.write_string(format!("    fn from(py_type: &{}) -> Self {{", self.struct_name));

            let is_box_type = impl_type.contains("Box<");
            if is_box_type {
                self.write_string(format!("        Box::new(flat::{} {{", self.struct_t_name));
            } else {
                self.write_str("        Self {");
            }

            for variable_info in &self.types {
                let variable_name = &variable_info[0];
                let variable_type = variable_info[1].as_str();

                if variable_type.starts_with("Vec<") {
                    let inner_type = variable_type.trim_start_matches("Vec<").trim_end_matches('>');
                    if Self::BASE_TYPES.contains(&inner_type) {
                        self.file_contents
                            .push(Cow::Owned(format!("            {variable_name}: py_type.{variable_name},",)));
                    } else {
                        self.file_contents.push(Cow::Owned(format!(
                            "            {variable_name}: py_type.{variable_name}.iter().map(Into::into).collect(),",
                        )));
                    }
                } else if variable_type.starts_with("Option<") {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {variable_name}: py_type.{variable_name}.as_ref().map(Into::into),",
                    )));
                } else if variable_type == "String" {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {variable_name}: py_type.{variable_name}.clone(),",
                    )));
                } else if Self::BASE_TYPES.contains(&variable_type) {
                    self.file_contents
                        .push(Cow::Owned(format!("            {variable_name}: py_type.{variable_name},",)));
                } else {
                    self.file_contents.push(Cow::Owned(format!(
                        "            {variable_name}: (&py_type.{variable_name}).into(),",
                    )));
                }
            }

            if is_box_type {
                self.write_str("        })");
            } else {
                self.write_str("        }");
            }

            self.write_str("    }");
            self.write_str("}");
            self.write_str("");
        }
    }

    fn generate_new_method(&mut self) {
        match self.bind_type {
            PythonBindType::Enum => self.generate_enum_new_method(),
            PythonBindType::Struct => self.generate_struct_new_method(),
            PythonBindType::Union => self.generate_union_new_method(),
        }
    }

    fn generate_union_new_method(&mut self) {
        self.write_str("    #[new]");
        assert!(u8::try_from(self.types.len()).is_ok());

        let mut signature_parts = vec!["item_type=Default::default()".to_string()];

        for variable_info in &self.types {
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                continue;
            }

            let snake_case_name = &variable_info[2];

            signature_parts.push(format!("{}=None", snake_case_name));
        }

        self.write_string(format!("    #[pyo3(signature = ({}))]", signature_parts.join(", ")));
        self.write_str("    pub fn new(");

        self.write_string(format!("        item_type: {}Type,", self.struct_name));
        for variable_info in &self.types {
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                continue;
            }

            let snake_case_name = &variable_info[2];

            self.file_contents.push(Cow::Owned(format!(
                "        {snake_case_name}: Option<super::{variable_type}>,"
            )));
        }

        self.write_str("    ) -> Self {");
        self.write_str("        Self {");

        self.write_str("            item_type,");
        for variable_info in &self.types {
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                continue;
            }

            let snake_case_name = &variable_info[2];

            self.file_contents.push(Cow::Owned(format!("            {snake_case_name},")));
        }

        self.write_str("        }");
        self.write_str("    }");
    }

    fn generate_enum_new_method(&mut self) {
        self.write_str("    #[new]");
        assert!(u8::try_from(self.types.len()).is_ok());

        self.write_str("    #[pyo3(signature = (value=Default::default()))]");
        self.write_str("    pub fn new(value: u8) -> Self {");
        self.write_str("        match value {");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let variable_value = &variable_info[1];

            self.file_contents
                .push(Cow::Owned(format!("            {variable_value} => Self::{variable_name},",)));
        }

        if self.types.len() != usize::from(u8::MAX) {
            self.write_str("            v => panic!(\"Unknown value: {v}\"),");
        }

        self.write_str("        }");
        self.write_str("    }");
    }

    fn generate_struct_new_method(&mut self) {
        self.write_str("    #[new]");

        let mut signature_parts = Vec::new();

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                continue;
            }

            if variable_type.starts_with("Option<") {
                signature_parts.push(format!("{}=None", variable_name));
            } else {
                signature_parts.push(format!("{}=Default::default()", variable_name));
            }
        }

        self.write_string(format!("    #[pyo3(signature = ({}))]", signature_parts.join(", ")));

        self.write_str("    pub fn new(");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let mut variable_type = variable_info[1].to_string();

            if variable_type.starts_with("Vec<") && variable_type.ends_with("T>") {
                variable_type = format!(
                    "Vec<super::{}>",
                    variable_type.trim_start_matches("Vec<").trim_end_matches("T>")
                );
            } else if variable_type.starts_with("Box<") && variable_type.ends_with('>') {
                variable_type = format!(
                    "super::{}",
                    variable_type
                        .trim_start_matches("Box<")
                        .trim_end_matches('>')
                        .trim_end_matches('T')
                );
            } else if variable_type.starts_with("Option<") && variable_type.ends_with('>') {
                let inner_type = variable_type
                    .trim_start_matches("Option<")
                    .trim_start_matches("Box<")
                    .trim_end_matches('>')
                    .trim_end_matches('T');

                if Self::BASE_TYPES.contains(&inner_type) {
                    variable_type = format!("Option<{inner_type}>");
                } else {
                    variable_type = format!("Option<super::{inner_type}>");
                }
            } else if variable_type.ends_with('T') || !Self::BASE_TYPES.contains(&variable_type.as_str()) {
                variable_type = format!("super::{}", variable_type.trim_end_matches('T'));
            }

            self.file_contents
                .push(Cow::Owned(format!("        {variable_name}: {variable_type},",)));
        }

        self.write_str("    ) -> Self {");
        self.write_str("        Self {");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];

            self.file_contents.push(Cow::Owned(format!("            {variable_name},",)));
        }

        self.write_str("        }");
        self.write_str("    }");
    }

    fn generate_str_method(&mut self) {
        self.write_str("    pub fn __str__(&self) -> String {");
        self.write_str("        format!(\"{self:?}\")");
        self.write_str("    }");
    }

    fn generate_repr_method(&mut self) {
        match self.bind_type {
            PythonBindType::Enum => self.generate_enum_repr_method(),
            PythonBindType::Struct => self.generate_struct_repr_method(),
            PythonBindType::Union => self.generate_union_repr_method(),
        }
    }

    fn generate_union_repr_method(&mut self) {
        self.write_str("    pub fn __repr__(&self) -> String {");
        self.write_str("        match self.item_type {");

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let variable_type = &variable_info[1];

            if variable_type.is_empty() {
                self.file_contents.push(Cow::Owned(format!(
                    "            {}Type::NONE => format!(\"{}(item_type={{:?}})\", self.item_type),",
                    self.struct_name, self.struct_name
                )));
            } else {
                let snake_case_name = &variable_info[2];

                self.file_contents.push(Cow::Owned(format!(
                    "            {}Type::{variable_name} => format!(",
                    self.struct_name
                )));
                self.file_contents.push(Cow::Owned(format!(
                    "                \"{}(item_type={{:?}}, {snake_case_name}={{:?}})\",",
                    self.struct_name
                )));
                self.file_contents.push(Cow::Owned(
                    format!("                self.item_type, self.{snake_case_name},",),
                ));
                self.file_contents.push(Cow::Borrowed("            ),"));
            }
        }

        self.write_str("        }");
        self.write_str("    }");
    }

    fn generate_enum_repr_method(&mut self) {
        self.write_str("    pub fn __repr__(&self) -> String {");
        self.write_string(format!("        format!(\"{}(value={{}})\", *self as u8)", self.struct_name));
        self.write_str("    }");
    }

    fn generate_struct_repr_method(&mut self) {
        self.write_str("    pub fn __repr__(&self) -> String {");
        self.write_str("        format!(");

        let repr_signature = self
            .types
            .iter()
            .map(|variable_info| {
                let variable_name = &variable_info[0];
                let variable_type = variable_info[1].as_str();

                if Self::BASE_TYPES.contains(&variable_type) {
                    format!("{variable_name}={{}}")
                } else {
                    format!("{variable_name}={{:?}}")
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        self.write_string(format!("            \"{}({repr_signature})\",", self.struct_name));

        for variable_info in &self.types {
            let variable_name = &variable_info[0];
            let variable_type = &variable_info[1];

            if variable_type == "bool" {
                self.file_contents
                    .push(Cow::Owned(format!("            crate::bool_to_str(self.{variable_name}),",)));
            } else {
                self.file_contents
                    .push(Cow::Owned(format!("            self.{variable_name},",)));
            }
        }

        self.write_str("        )");
        self.write_str("    }");
    }

    fn generate_pack_method(&mut self) {
        self.write_str("    fn pack<'p>(&self, py: Python<'p>) -> &'p PyBytes {");
        if self.has_complex_pack {
            self.write_str("        let size = self.get_size();");
            self.write_str("        let mut builder = FlatBufferBuilder::with_capacity(size);");
            self.write_str("");
        }

        let name = if self.bind_type == PythonBindType::Enum {
            &self.struct_name
        } else {
            &self.struct_t_name
        };
        self.write_string(format!("        let flat_t = flat::{name}::from(self);"));

        if self.has_complex_pack {
            self.write_str("        let offset = flat_t.pack(&mut builder);");
            self.write_str("        builder.finish(offset, None);");
            self.write_str("");
            self.write_str("        PyBytes::new(py, builder.finished_data())");
        } else if self.bind_type == PythonBindType::Enum {
            self.write_str("        PyBytes::new(py, &[flat_t.0])");
        } else {
            self.write_str("        let item = flat_t.pack();");
            self.write_str("");
            self.write_str("        PyBytes::new(py, &item.0)");
        }

        self.write_str("    }");
    }

    fn generate_unpack_method(&mut self) {
        self.write_str("    #[staticmethod]");
        self.write_str("    fn unpack(data: &[u8]) -> Self {");

        if self.bind_type == PythonBindType::Enum {
            self.write_string(format!("        root::<flat::{}>(data).unwrap().into()", self.struct_name));
        } else {
            self.write_string(format!(
                "        root::<flat::{}>(data).unwrap().unpack().into()",
                self.struct_name
            ));
        }

        self.write_str("    }");
    }

    fn generate_py_methods(&mut self) {
        self.write_str("#[pymethods]");
        self.write_string(format!("impl {} {{", self.struct_name));

        self.generate_new_method();
        self.write_str("");
        self.generate_str_method();
        self.write_str("");
        self.generate_repr_method();

        if self.bind_type != PythonBindType::Union {
            self.write_str("");
            self.generate_pack_method();
            self.write_str("");
            self.generate_unpack_method();
        }

        self.write_str("}");
        self.write_str("");
    }

    fn finish(self) -> io::Result<(String, String, Vec<Vec<String>>)> {
        let file_path = format!("{PYTHON_OUT_FOLDER}/{}.rs", self.filename);
        let file = Path::new(&file_path);

        fs::create_dir_all(file.parent().unwrap())?;
        fs::write(file, self.file_contents.join("\n"))?;

        Ok((self.filename, self.struct_name, self.types))
    }
}

fn camel_to_snake_case(variable_name: &str) -> String {
    let mut snake_case_parts: Vec<String> = Vec::new();

    let mut last_was_uppercase = false;
    for c in variable_name.chars() {
        if c.is_uppercase() {
            if !last_was_uppercase {
                snake_case_parts.push(c.to_lowercase().to_string());
            } else {
                snake_case_parts.last_mut().unwrap().push(c.to_lowercase().next().unwrap());
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

fn mod_rs_generator(type_data: &[(String, String, Vec<Vec<String>>)]) -> io::Result<()> {
    let mut file_contents = Vec::new();

    for (filename, _, _) in type_data {
        file_contents.push(Cow::Owned(format!("mod {filename};")));
        file_contents.push(Cow::Owned(format!("pub use {filename}::*;")));
    }

    file_contents.push(Cow::Borrowed(""));

    fs::write(format!("{PYTHON_OUT_FOLDER}/mod.rs"), file_contents.join("\n"))?;

    Ok(())
}

fn class_names_txt_generator(type_data: &[(String, String, Vec<Vec<String>>)]) -> io::Result<()> {
    let mut file_contents = vec!["[".to_string()];

    for (_, type_name, _) in type_data {
        file_contents.push(format!("    {type_name},"));
    }

    file_contents.push("]".to_string());

    fs::write("classes.txt", file_contents.join("\n"))?;

    Ok(())
}

fn pyi_generator(type_data: &[(String, String, Vec<Vec<String>>)]) -> io::Result<()> {
    let mut file_contents = vec![
        Cow::Borrowed("from __future__ import annotations"),
        Cow::Borrowed(""),
        Cow::Borrowed("from enum import Enum"),
        Cow::Borrowed("from typing import Optional"),
        Cow::Borrowed(""),
        Cow::Borrowed("__doc__: str"),
        Cow::Borrowed("__version__: str"),
        Cow::Borrowed(""),
    ];

    let primitive_map = [
        ("bool", "bool"),
        ("i32", "int"),
        ("f32", "float"),
        ("String", "str"),
        ("u8", "int"),
    ];

    for (_, type_name, types) in type_data {
        let is_enum = !types.is_empty()
            && types
                .iter()
                .enumerate()
                .all(|(i, variable_info)| variable_info[1] == i.to_string());

        let is_union = !types.is_empty() && types[0][1].is_empty();

        if is_union {
            file_contents.push(Cow::Owned(format!("class {type_name}Type(Enum):")));

            for (i, variable_info) in types.iter().enumerate() {
                let variable_name = variable_info[0].as_str();
                file_contents.push(Cow::Owned(format!("    {variable_name} = {i}",)));
            }

            file_contents.push(Cow::Borrowed(""));
        }

        file_contents.push(Cow::Owned(format!("class {type_name}:")));

        if is_union {
            file_contents.push(Cow::Owned(format!("    item_type: {}Type", type_name)));
        }

        'outer: for variable_info in types {
            let mut variable_name = &variable_info[0];

            if variable_name == "NONE" {
                continue;
            } else if is_union {
                variable_name = &variable_info[2];
            }

            let variable_type = if is_union {
                format!("Option<{}>", variable_info[1])
            } else {
                variable_info[1].clone()
            };

            if is_enum {
                file_contents.push(Cow::Owned(format!("    {variable_name} = {variable_type}",)));
                continue;
            }

            for (rust_type, python_type) in primitive_map {
                if variable_type == rust_type {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: {python_type}")));
                    continue 'outer;
                }
            }

            if variable_type.starts_with("Vec<") {
                let type_name = variable_type
                    .trim_start_matches("Vec<")
                    .trim_end_matches('>')
                    .trim_end_matches('T');
                file_contents.push(Cow::Owned(format!("    {variable_name}: list[{type_name}]")));
            } else if variable_type.starts_with("Option<") {
                let type_name = variable_type
                    .trim_start_matches("Option<")
                    .trim_start_matches("Box<")
                    .trim_end_matches('>')
                    .trim_end_matches('T');

                if type_name == "bool" {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: Optional[bool]")));
                } else if type_name == "i32" {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: Optional[int]")));
                } else if type_name == "f32" {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: Optional[float]")));
                } else if type_name == "String" {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: Optional[str]")));
                } else {
                    file_contents.push(Cow::Owned(format!("    {variable_name}: Optional[{type_name}]")));
                }
            } else if variable_type.starts_with("Box<") && variable_type.ends_with("T>") {
                let type_name = variable_type.trim_start_matches("Box<").trim_end_matches("T>");
                file_contents.push(Cow::Owned(format!("    {variable_name}: {type_name}")));
            } else if variable_type.ends_with('T') {
                let type_name = variable_type.trim_end_matches('T');
                file_contents.push(Cow::Owned(format!("    {variable_name}: {type_name}")));
            } else {
                file_contents.push(Cow::Owned(format!("    {variable_name}: {variable_type}")));
            }
        }

        file_contents.push(Cow::Borrowed(""));

        if is_enum {
            file_contents.push(Cow::Borrowed("    def __init__(self, value: int = 0): ..."));
        } else {
            file_contents.push(Cow::Borrowed("    def __init__("));
            file_contents.push(Cow::Borrowed("        self,"));

            for variable_info in types {
                if &variable_info[0] == "NONE" {
                    continue;
                }

                let variable_name = if is_union { &variable_info[2] } else { &variable_info[0] };

                let variable_type = if is_union {
                    format!("Option<{}>", variable_info[1])
                } else {
                    variable_info[1].clone()
                };

                let default_value = match variable_type.as_str() {
                    "bool" => Cow::Borrowed("False"),
                    "i32" | "f32" | "u8" => Cow::Borrowed("0"),
                    "String" => Cow::Borrowed("\"\""),
                    t => {
                        if t.starts_with("Vec<") {
                            Cow::Borrowed("[]")
                        } else if t.starts_with("Box<") {
                            let inner_type = t.trim_start_matches("Box<").trim_end_matches('>').trim_end_matches('T');
                            Cow::Owned(format!("{}()", inner_type))
                        } else if t.starts_with("Option<") {
                            Cow::Borrowed("None")
                        } else {
                            Cow::Owned(format!("{}()", t.trim_end_matches('T')))
                        }
                    }
                };

                file_contents.push(Cow::Owned(format!("        {variable_name}={default_value},")));
            }

            file_contents.push(Cow::Borrowed("    ): ..."));
        }

        file_contents.push(Cow::Borrowed("    def __str__(self) -> str: ..."));
        file_contents.push(Cow::Borrowed("    def __repr__(self) -> str: ..."));

        if !is_union {
            file_contents.push(Cow::Borrowed("    def pack(self) -> bytes: ..."));
            file_contents.push(Cow::Borrowed("    @staticmethod"));
            file_contents.push(Cow::Owned(format!("    def unpack(data: bytes) -> {type_name}: ...")));
        }

        file_contents.push(Cow::Borrowed(""));
    }

    fs::write("rlbot_flatbuffers.pyi", file_contents.join("\n"))?;

    Ok(())
}

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=flatbuffers-schema/comms.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/event.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/gamestate.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/matchstart.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/rendering.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/rlbot.fbs");

    set_current_dir(env!("CARGO_MANIFEST_DIR"))?;

    let mut schema_folder = Path::new(SCHEMA_FOLDER);
    if !schema_folder.exists() {
        schema_folder = Path::new(SCHEMA_FOLDER_BACKUP);
        assert!(schema_folder.exists(), "Could not find flatbuffers schema folder");
    }

    let schema_folder_str = schema_folder.display();

    Command::new(format!("{schema_folder_str}/{FLATC_BINARY}"))
        .args([
            "--rust",
            "--gen-object-api",
            "--gen-all",
            "--filename-suffix",
            "",
            "--rust-module-root-file",
            "-o",
            OUT_FOLDER,
            &format!("{schema_folder_str}/rlbot.fbs"),
        ])
        .spawn()?
        .wait()?;

    let out_folder = Path::new(OUT_FOLDER).join("rlbot").join("flat");

    assert!(out_folder.exists(), "Could not find generated folder: {}", out_folder.display());

    // ^ the above generates the Rust flatbuffers code,
    // and the below we generates the wanted additional Python binds

    // read the current contents of the generated folder
    let generated_files = fs::read_dir(out_folder)?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, io::Error>>()?;

    let mut type_data = Vec::new();

    for path in generated_files {
        let Some(mut python_bind_generator) = PythonBindGenerator::new(&path) else {
            continue;
        };

        python_bind_generator.generate_definition();
        python_bind_generator.generate_from_flat_impls();
        python_bind_generator.generate_to_flat_impls();
        python_bind_generator.generate_py_methods();

        type_data.push(python_bind_generator.finish()?);
    }

    mod_rs_generator(&type_data)?;
    pyi_generator(&type_data)?;
    class_names_txt_generator(&type_data)?;

    Ok(())
}
