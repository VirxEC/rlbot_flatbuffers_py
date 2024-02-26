#[allow(
    clippy::too_many_arguments,
    clippy::useless_format,
    clippy::upper_case_acronyms,
    non_camel_case_types,
    unused_variables
)]
mod python;

#[allow(clippy::all, non_snake_case, unused_imports)]
pub mod generated;

use pyo3::prelude::*;
use python::*;

#[must_use]
pub const fn bool_to_str(b: bool) -> &'static str {
    if b {
        "True"
    } else {
        "False"
    }
}

macro_rules! pynamedmodule {
    (doc: $doc:literal, name: $name:tt, classes: [$($class_name:ident),*], vars: [$(($var_name:literal, $value:expr)),*]) => {
        #[doc = $doc]
        #[pymodule]
        #[allow(redundant_semicolons)]
        fn $name(_py: Python, m: &PyModule) -> PyResult<()> {
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
