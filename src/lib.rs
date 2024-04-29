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

use pyo3::{prelude::*, types::PyBytes, PyClass};
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

pub fn get_empty_pybytes() -> Py<PyBytes> {
    Python::with_gil(|py| PyBytes::new_bound(py, &[]).unbind())
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
        fn $name(m: Bound<PyModule>) -> PyResult<()> {
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
        AirState,
        BallBouncinessOption,
        BallInfo,
        BallMaxSpeedOption,
        BallPrediction,
        BallSizeOption,
        BallTypeOption,
        BallWeightOption,
        Bool,
        BoostOption,
        BoostPad,
        BoostPadState,
        BoostStrengthOption,
        BoxShape,
        CollisionShape,
        Color,
        ConsoleCommand,
        ControllerState,
        CylinderShape,
        DemolishOption,
        DesiredBallState,
        DesiredBoostState,
        DesiredCarState,
        DesiredGameInfoState,
        DesiredGameState,
        DesiredPhysics,
        ExistingMatchBehavior,
        FieldInfo,
        Float,
        GameInfo,
        GameMessage,
        GameMessageWrapper,
        GameMode,
        GameSpeedOption,
        GameStateType,
        GameTickPacket,
        GoalInfo,
        GravityOption,
        Human,
        Launcher,
        Line3D,
        LoadoutPaint,
        MatchComm,
        MatchLength,
        MatchSettings,
        MaxScore,
        MessagePacket,
        MutatorSettings,
        OvertimeOption,
        PartyMember,
        Physics,
        PlayerClass,
        PlayerConfiguration,
        PlayerInfo,
        PlayerInput,
        PlayerInputChange,
        PlayerLoadout,
        PlayerSpectate,
        PlayerStatEvent,
        PolyLine3D,
        PredictionSlice,
        Psyonix,
        RLBot,
        ReadyMessage,
        RemoveRenderGroup,
        RenderGroup,
        RenderMessage,
        RenderType,
        RespawnTimeOption,
        Rotator,
        RotatorPartial,
        RumbleOption,
        ScoreInfo,
        ScriptConfiguration,
        SeriesLengthOption,
        SphereShape,
        StartCommand,
        StopCommand,
        String2D,
        String3D,
        TeamInfo,
        TextHAlign,
        TextVAlign,
        Touch,
        Vector3,
        Vector3Partial
    ],
    vars: [
        ("__version__", env!("CARGO_PKG_VERSION"))
    ]
}
