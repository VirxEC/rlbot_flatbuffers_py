use crate::PYTHON_OUT_FOLDER;
use std::{borrow::Cow, fs, io, path::Path};

pub trait Generator {
    fn filename(&self) -> &str;
    fn struct_name(&self) -> &str;
    fn file_contents(&self) -> &Vec<Cow<'static, str>>;

    fn modify_source(&self, path: &Path);
    fn generate_definition(&mut self);
    fn generate_from_flat_impls(&mut self);
    fn generate_to_flat_impls(&mut self);
    fn generate_py_methods(&mut self);

    fn finish(&self) -> io::Result<()> {
        let file_path = format!("{PYTHON_OUT_FOLDER}/{}.rs", self.filename());
        let file = Path::new(&file_path);

        fs::create_dir_all(file.parent().unwrap())?;
        fs::write(file, self.file_contents().join("\n"))?;

        Ok(())
    }

    fn generate(&mut self, filepath: &Path) -> io::Result<()> {
        self.modify_source(filepath);
        self.generate_definition();
        self.generate_from_flat_impls();
        self.generate_to_flat_impls();
        self.generate_py_methods();
        self.finish()?;

        Ok(())
    }
}
