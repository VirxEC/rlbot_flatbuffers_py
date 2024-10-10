mod class_inject;
mod enums;
mod generator;
mod pyi;
mod structs;
mod unions;

use std::{borrow::Cow, env::set_current_dir, fs, io, path::Path, process::Command};

use generator::Generator;
use structs::StructBindGenerator;

const FLATC_BINARY: &str = if cfg!(windows) { "flatc.exe" } else { "flatc" };
const OUT_FOLDER: &str = "./src/generated";
const SCHEMA_FOLDER: &str = "./flatbuffers-schema";
const SCHEMA_FOLDER_BACKUP: &str = "../flatbuffers-schema";

pub const PYTHON_OUT_FOLDER: &str = "./src/python";

pub enum PythonBindType {
    Struct(structs::StructBindGenerator),
    Enum(enums::EnumBindGenerator),
    Union(unions::UnionBindGenerator),
}

impl PythonBindType {
    pub const BASE_TYPES: [&'static str; 6] = ["bool", "i32", "u32", "f32", "String", "u8"];
    pub const FROZEN_TYPES: [&'static str; 21] = [
        "FieldInfo",
        "BoostPad",
        "GoalInfo",
        "GamePacket",
        "PlayerInfo",
        "ScoreInfo",
        "BallInfo",
        "Touch",
        "CollisionShape",
        "BoxShape",
        "SphereShape",
        "CylinderShape",
        "BoostPadState",
        "GameInfo",
        "TeamInfo",
        "BallPrediction",
        "PredictionSlice",
        "Physics",
        "Vector2",
        "ControllableInfo",
        "ControllableTeamInfo",
    ];
    pub const FROZEN_NEEDS_PY: [&'static str; 3] = ["GamePacket", "BallInfo", "CollisionShape"];
    pub const UNIONS: [&'static str; 4] = [
        "PlayerClass",
        "CollisionShape",
        "RelativeAnchor",
        "RenderType",
    ];

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

        if let Some(types) = StructBindGenerator::get_types(&contents, &struct_t_name) {
            return Some(Self::Struct(StructBindGenerator::new(
                filename.to_string(),
                struct_name,
                struct_t_name,
                contents,
                types,
            )?));
        }

        if let Some((types, enum_type)) =
            enums::EnumBindGenerator::get_types(&contents, &struct_name)
        {
            return Some(match enum_type {
                enums::EnumType::Enum => Self::Enum(enums::EnumBindGenerator::new(
                    filename.to_string(),
                    struct_name,
                    types,
                )?),
                enums::EnumType::Union => Self::Union(unions::UnionBindGenerator::new(
                    filename.to_string(),
                    struct_name,
                    struct_t_name,
                    types,
                )?),
            });
        }

        None
    }

    pub fn filename(&self) -> &str {
        match self {
            Self::Struct(gen) => gen.filename(),
            Self::Enum(gen) => gen.filename(),
            Self::Union(gen) => gen.filename(),
        }
    }

    pub fn struct_name(&self) -> &str {
        match self {
            Self::Struct(gen) => gen.struct_name(),
            Self::Enum(gen) => gen.struct_name(),
            Self::Union(gen) => gen.struct_name(),
        }
    }

    pub fn generate(&mut self, filepath: &Path) -> io::Result<()> {
        match self {
            Self::Struct(gen) => gen.generate(filepath),
            Self::Enum(gen) => gen.generate(filepath),
            Self::Union(gen) => gen.generate(filepath),
        }
    }
}

fn mod_rs_generator(type_data: &[PythonBindType]) -> io::Result<()> {
    let mut file_contents = Vec::new();

    for generator in type_data {
        let filename = generator.filename();

        file_contents.push(Cow::Owned(format!("mod {filename};")));
        file_contents.push(Cow::Owned(format!("pub use {filename}::*;")));
    }

    file_contents.push(Cow::Borrowed(""));

    fs::write(
        format!("{PYTHON_OUT_FOLDER}/mod.rs"),
        file_contents.join("\n"),
    )?;

    Ok(())
}

fn run_flatc() -> io::Result<()> {
    println!("cargo:rerun-if-changed=flatbuffers-schema/comms.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/gamestate.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/matchstart.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/rendering.fbs");
    println!("cargo:rerun-if-changed=flatbuffers-schema/rlbot.fbs");

    set_current_dir(env!("CARGO_MANIFEST_DIR"))?;

    let mut schema_folder = Path::new(SCHEMA_FOLDER);
    if !schema_folder.exists() {
        schema_folder = Path::new(SCHEMA_FOLDER_BACKUP);
        assert!(
            schema_folder.exists(),
            "Could not find flatbuffers schema folder"
        );
    }

    let schema_folder_str = schema_folder.display();

    let mut proc = Command::new(format!("{schema_folder_str}/{FLATC_BINARY}"));

    proc.args([
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

    assert!(proc.status()?.success(), "flatc failed to run");

    Ok(())
}

fn main() -> io::Result<()> {
    run_flatc()?;

    let out_folder = Path::new(OUT_FOLDER).join("rlbot").join("flat");

    assert!(
        out_folder.exists(),
        "Could not find generated folder: {}",
        out_folder.display()
    );

    // read the current contents of the generated folder
    let generated_files = fs::read_dir(out_folder)?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, io::Error>>()?;

    let mut type_data = Vec::new();

    for path in generated_files {
        let Some(mut bind_generator) = PythonBindType::new(&path) else {
            continue;
        };

        bind_generator.generate(&path)?;
        type_data.push(bind_generator);
    }

    mod_rs_generator(&type_data)?;
    pyi::generator(&type_data)?;
    class_inject::classes_to_lib_rs(&type_data)?;

    Ok(())
}
