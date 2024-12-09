#[allow(
    clippy::extra_unused_lifetimes,
    clippy::missing_safety_doc,
    clippy::derivable_impls,
    clippy::unnecessary_cast,
    clippy::size_of_in_element_count,
    clippy::needless_lifetimes,
    clippy::too_long_first_doc_paragraph,
    non_snake_case,
    unused_imports
)]
pub mod generated;

#[allow(clippy::enum_variant_names, unused_imports)]
mod python;

use pyo3::{create_exception, exceptions::PyValueError, prelude::*, PyClass};
use python::*;
use std::panic::Location;

create_exception!(
    rlbot_flatbuffers,
    InvalidFlatbuffer,
    PyValueError,
    "Invalid FlatBuffer"
);

#[inline(never)]
#[track_caller]
pub fn flat_err_to_py(err: flatbuffers::InvalidFlatbuffer) -> PyErr {
    let caller = Location::caller();
    let err_msg = format!(
        "Can't make flatbuffer @ \"rlbot_flatbuffers/{}\":\n  {err}",
        caller.file()
    );
    InvalidFlatbuffer::new_err(err_msg)
}

pub trait FromGil<T> {
    fn from_gil(py: Python, obj: T) -> Self;
}

impl<T, U> FromGil<T> for U
where
    U: From<T>,
{
    #[inline]
    fn from_gil(_py: Python, obj: T) -> Self {
        Self::from(obj)
    }
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

#[inline(never)]
fn into_py_from<T, U>(py: Python, obj: T) -> Py<U>
where
    T: IntoGil<U>,
    U: pyo3::PyClass + Into<PyClassInitializer<U>>,
{
    Py::new(py, obj.into_gil(py)).unwrap()
}

#[inline(never)]
fn from_py_into<T, U>(py: Python, obj: &Py<T>) -> U
where
    T: PyClass,
    U: for<'a> FromGil<&'a T>,
{
    (&*obj.borrow(py)).into_gil(py)
}

pub trait PyDefault: Sized + PyClass {
    fn py_default(py: Python) -> Py<Self>;
}

impl<T: Default + PyClass + Into<PyClassInitializer<T>>> PyDefault for T {
    fn py_default(py: Python) -> Py<Self> {
        Py::new(py, Self::default()).unwrap()
    }
}

static NONE_STR: &str = "None";

#[must_use]
#[inline(never)]
pub fn none_str() -> String {
    String::from(NONE_STR)
}

#[must_use]
#[inline(never)]
pub const fn bool_to_str(b: bool) -> &'static str {
    if b {
        "True"
    } else {
        "False"
    }
}

#[derive(Debug, FromPyObject)]
pub enum Floats {
    Num(f32),
    Flat(Py<Float>),
}

impl FromGil<Floats> for Py<Float> {
    fn from_gil(py: Python, floats: Floats) -> Self {
        match floats {
            Floats::Flat(float) => float,
            Floats::Num(num) => Py::new(py, Float::new(num)).unwrap(),
        }
    }
}

#[derive(Debug, FromPyObject)]
pub enum Bools {
    Num(bool),
    Flat(Py<Bool>),
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
    (doc: $doc:literal, name: $name:tt, classes: [$($class_name:ident),*], vars: [$(($var_name:literal, $value:expr)),*], exceptions: [$($except:expr),*]) => {
        #[doc = $doc]
        #[pymodule]
        #[allow(redundant_semicolons)]
        fn $name(py: Python, m: Bound<PyModule>) -> PyResult<()> {
            $(m.add_class::<$class_name>()?);*;
            $(m.add($var_name, $value)?);*;
            $(m.add(stringify!($except), py.get_type::<$except>())?);*;
            Ok(())
        }
    };
}

pynamedmodule! {
    doc: "rlbot_flatbuffers is a Python module implemented in Rust for serializing and deserializing RLBot's flatbuffers.",
    name: rlbot_flatbuffers,
    classes: [
        AirState,
        AudioOption,
        BallAnchor,
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
        CarAnchor,
        CollisionShape,
        Color,
        ConnectionSettings,
        ConsoleCommand,
        ControllableInfo,
        ControllableTeamInfo,
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
        GameEventOption,
        GameInfo,
        GameMode,
        GamePacket,
        GameSpeedOption,
        GameStatus,
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
        MaxTimeOption,
        MultiBall,
        MutatorSettings,
        OvertimeOption,
        PartyMember,
        Physics,
        PlayerClass,
        PlayerConfiguration,
        PlayerInfo,
        PlayerInput,
        PlayerLoadout,
        PolyLine3D,
        PredictionSlice,
        Psyonix,
        PsyonixSkill,
        RLBot,
        Rect2D,
        Rect3D,
        RelativeAnchor,
        RemoveRenderGroup,
        RenderAnchor,
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
        SetLoadout,
        SphereShape,
        StartCommand,
        StopCommand,
        String2D,
        String3D,
        TeamInfo,
        TextHAlign,
        TextVAlign,
        Touch,
        Vector2,
        Vector3,
        Vector3Partial
    ],
    vars: [
        ("__version__", env!("CARGO_PKG_VERSION"))
    ],
    exceptions: [
        InvalidFlatbuffer
    ]
}
