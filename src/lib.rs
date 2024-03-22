#[allow(
    clippy::extra_unused_lifetimes,
    clippy::missing_safety_doc,
    clippy::derivable_impls,
    clippy::unnecessary_cast,
    clippy::size_of_in_element_count,
    non_snake_case,
    unused_imports
)]
pub mod generated;

#[allow(clippy::enum_variant_names)]
mod python;

use pyo3::{prelude::*, PyClass};
use python::*;

pub trait FromGil<T> {
    fn from_gil(py: Python, obj: T) -> Self;
}

pub trait IntoGil<T>: Sized {
    fn into_gil(self, py: Python) -> T;
}

impl<T, U> IntoGil<U> for T
where
    U: FromGil<T>,
{
    #[inline]
    fn into_gil(self, py: Python) -> U {
        U::from_gil(py, self)
    }
}

pub trait PyDefault: Sized + PyClass {
    fn py_default(py: Python) -> Py<Self>;
}

impl<T: Default + PyClass + Into<PyClassInitializer<T>>> PyDefault for T {
    fn py_default(py: Python) -> Py<Self> {
        Py::new(py, Self::default()).unwrap()
    }
}

pub fn get_py_default<T: PyDefault>() -> Py<T> {
    Python::with_gil(|py| T::py_default(py))
}

#[must_use]
pub fn none_str() -> String {
    String::from("None")
}

#[must_use]
pub const fn bool_to_str(b: bool) -> &'static str {
    if b {
        "True"
    } else {
        "False"
    }
}

#[derive(Debug, Clone, FromPyObject)]
pub enum Floats {
    Flat(Py<Float>),
    Num(f32),
}

impl Default for Floats {
    fn default() -> Self {
        Floats::Flat(get_py_default())
    }
}

impl FromGil<Floats> for Py<Float> {
    fn from_gil(py: Python, floats: Floats) -> Self {
        match floats {
            Floats::Flat(float) => float,
            Floats::Num(num) => Py::new(py, Float::new(num)).unwrap(),
        }
    }
}

#[derive(Debug, Clone, FromPyObject)]
pub enum Bools {
    Flat(Py<Bool>),
    Num(bool),
}

impl Default for Bools {
    fn default() -> Self {
        Self::Flat(get_py_default())
    }
}

impl FromGil<Bools> for Py<Bool> {
    fn from_gil(py: Python, bools: Bools) -> Self {
        match bools {
            Bools::Flat(float) => float,
            Bools::Num(num) => Py::new(py, Bool::new(num)).unwrap(),
        }
    }
}

macro_rules! pynamedmodule {
    (doc: $doc:literal, name: $name:tt, classes: [$($class_name:ident),*], vars: [$(($var_name:literal, $value:expr)),*]) => {
        #[doc = $doc]
        #[pymodule]
        #[allow(redundant_semicolons)]
        fn $name(_py: Python, m: Bound<PyModule>) -> PyResult<()> {
            $(m.add_class::<$class_name>()?);*;
            $(m.add($var_name, $value)?);*;
            Ok(())
        }
    };
}

pynamedmodule! {
    doc: "rlbot_flatbuffers is a Python module implemented in Rust for serializing and deserializing RLBot's flatbuffers.",
    name: rlbot_flatbuffers,
    classes: [
        MessagePacket,
        PredictionSlice,
        SphereShape,
        PartyMember,
        GameMode,
        Physics,
        PlayerStatEvent,
        RenderMessage,
        GameStateType,
        GameMessage,
        BallPrediction,
        MutatorSettings,
        Vector3,
        RotatorPartial,
        DesiredBallState,
        PlayerInputChange,
        BallSizeOption,
        ScoreInfo,
        PlayerLoadout,
        PlayerConfiguration,
        BoostStrengthOption,
        ReadyMessage,
        Line3D,
        PlayerClass,
        CollisionShape,
        Color,
        Float,
        MatchLength,
        RLBot,
        RemoveRenderGroup,
        DesiredGameInfoState,
        GravityOption,
        Bool,
        DemolishOption,
        GameTickPacket,
        Psyonix,
        GameMessageWrapper,
        OvertimeOption,
        BoostOption,
        DesiredCarState,
        Vector3Partial,
        PlayerSpectate,
        AirState,
        Launcher,
        String3D,
        BallInfo,
        ExistingMatchBehavior,
        RenderGroup,
        PlayerInput,
        StartCommand,
        FieldInfo,
        ConsoleCommand,
        SeriesLengthOption,
        BallTypeOption,
        DesiredGameState,
        String2D,
        StopCommand,
        DesiredBoostState,
        Human,
        DesiredPhysics,
        ScriptConfiguration,
        ControllerState,
        CylinderShape,
        BallBouncinessOption,
        TeamInfo,
        PolyLine3D,
        BallWeightOption,
        LoadoutPaint,
        BoxShape,
        BoostPad,
        MatchSettings,
        BallMaxSpeedOption,
        GameInfo,
        Touch,
        RumbleOption,
        TextVAlign,
        RenderType,
        GoalInfo,
        TextHAlign,
        GameSpeedOption,
        MaxScore,
        BoostPadState,
        PlayerInfo,
        RespawnTimeOption,
        Rotator
    ],
    vars: [
        ("__version__", env!("CARGO_PKG_VERSION"))
    ]
}
