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
        ExistingMatchBehavior,
        PlayerStatEvent,
        DesiredCarState,
        RemoveRenderGroup,
        BallPrediction,
        PlayerInfo,
        GameStateType,
        GameSpeedOption,
        ScoreInfo,
        MessagePacket,
        DemolishOption,
        Vector3Partial,
        TextHAlign,
        BoostOption,
        RumbleOption,
        Vector3,
        GravityOption,
        BallInfo,
        ReadyMessage,
        StartCommand,
        MatchSettings,
        OvertimeOption,
        PlayerSpectate,
        SphereShape,
        PsyonixBotPlayer,
        Touch,
        TextVAlign,
        ConsoleCommand,
        DesiredGameInfoState,
        PredictionSlice,
        PolyLine3D,
        Rotator,
        BallTypeOption,
        LoadoutPaint,
        RotatorPartial,
        PartyMemberBotPlayer,
        CylinderShape,
        PlayerInputChange,
        DesiredBallState,
        CollisionShape,
        RenderMessage,
        BoostPadState,
        GameMode,
        ControllerState,
        BoostStrengthOption,
        GoalInfo,
        BallBouncinessOption,
        MaxScore,
        SeriesLengthOption,
        GameMessage,
        DesiredBoostState,
        BallSizeOption,
        BallMaxSpeedOption,
        MatchLength,
        BallWeightOption,
        PlayerLoadout,
        DesiredPhysics,
        AirState,
        GameMessageWrapper,
        GameTickPacket,
        String3D,
        Float,
        FieldInfo,
        PlayerClass,
        BoostPad,
        Bool,
        RespawnTimeOption,
        String2D,
        HumanPlayer,
        GameInfo,
        PlayerInput,
        Color,
        Line3D,
        Physics,
        MutatorSettings,
        PlayerConfiguration,
        DesiredGameState,
        TeamInfo,
        RenderType,
        RenderGroup,
        Launcher,
        BoxShape,
        RLBotPlayer
    ],
    vars: [
        ("__version__", env!("CARGO_PKG_VERSION"))
    ]
}
