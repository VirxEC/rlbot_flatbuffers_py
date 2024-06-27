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

#[allow(clippy::enum_variant_names, unused_imports)]
mod python;

use pyo3::{create_exception, exceptions::PyValueError, prelude::*, types::PyBytes, PyClass};
use python::*;
use std::panic::Location;

create_exception!(rlbot_flatbuffers, InvalidFlatbuffer, PyValueError, "Invalid FlatBuffer");

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

fn into_py_from<T, U>(py: Python, obj: T) -> Py<U>
where
    T: IntoGil<U>,
    U: pyo3::PyClass + Into<PyClassInitializer<U>>,
{
    Py::new(py, obj.into_gil(py)).unwrap()
}

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

#[derive(Debug, FromPyObject)]
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

#[derive(Debug, FromPyObject)]
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
    (doc: $doc:literal, name: $name:tt, classes: [$($class_name:ident),*], vars: [$(($var_name:literal, $value:expr)),*], exceptions: [$($except:expr),*]) => {
        #[doc = $doc]
        #[pymodule]
        #[allow(redundant_semicolons)]
        fn $name(py: Python, m: Bound<PyModule>) -> PyResult<()> {
            $(m.add_class::<$class_name>()?);*;
            $(m.add($var_name, $value)?);*;
            $(m.add(stringify!($except), py.get_type_bound::<$except>())?);*;
            Ok(())
        }
    };
}

pynamedmodule! {
    doc: "rlbot_flatbuffers is a Python module implemented in Rust for serializing and deserializing RLBot's flatbuffers.",
    name: rlbot_flatbuffers,
    classes: [
        AirState,
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
    ],
    exceptions: [
        InvalidFlatbuffer
    ]
}
