use std::{fs, io};

use crate::PythonBindType;

pub fn classes_to_lib_rs(type_data: &[PythonBindType]) -> io::Result<()> {
    let mut class_names = type_data
        .iter()
        .map(|generator| format!("        {}", generator.struct_name()))
        .collect::<Vec<_>>();
    class_names.sort();

    let file_contents = format!("    classes: [\n{}\n    ],", class_names.join(",\n"));

    let mut lib_rs = fs::read_to_string("src/lib.rs")?;

    #[cfg(windows)]
    {
        lib_rs = lib_rs.replace("\r\n", "\n");
    }

    let start = lib_rs.find("    classes: [\n").unwrap();
    let end = lib_rs[start..].find("],").unwrap() + 2;

    lib_rs.replace_range(start..start + end, &file_contents);

    fs::write("src/lib.rs", lib_rs)?;

    Ok(())
}
