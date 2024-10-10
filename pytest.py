from random import randrange
from time import time_ns

from rlbot_flatbuffers import *


class MyVector(Vector3):
    def __add__(self, other):
        return MyVector(self.x + other.x, self.y + other.y, self.z + other.z)


def random_string():
    return "".join(chr(randrange(32, 127)) for _ in range(64))


def random_player_config():
    return PlayerConfiguration(
        variety=Psyonix(PsyonixSkill.AllStar),
        name=random_string(),
        location=random_string(),
        run_command=random_string(),
        loadout=PlayerLoadout(
            loadout_paint=LoadoutPaint(),
            primary_color_lookup=Color(),
            secondary_color_lookup=Color(),
        ),
    )


def random_script_config():
    return ScriptConfiguration(
        location=random_string(),
        run_command=random_string(),
    )


if __name__ == "__main__":
    vec1 = MyVector(1, 2, 3)
    vec2 = Vector3(4, 5, 6)
    print(vec1 + vec2)

    player_info = PlayerInfo(accolades=["MVP", "Hat Trick"])
    eval(repr(player_info))
    print()

    connection_settings = ConnectionSettings("rlbot/abot", True, close_after_match=True)
    print(hash(connection_settings))
    print(connection_settings)
    eval(repr(connection_settings))
    print()

    dgs = DesiredGameState(
        game_info_state=DesiredGameInfoState(game_speed=2, end_match=Bool())
    )

    match dgs.game_info_state:
        case DesiredGameInfoState():
            dgs.game_info_state.world_gravity_z = Float(-650)
        case _:
            assert False

    match dgs.game_info_state.end_match:
        case Bool(val):
            dgs.game_info_state.end_match.val = not val
        case _:
            assert False

    dgs.console_commands = [ConsoleCommand("dump_items")]
    dgs.ball_states = [DesiredBallState()]

    print(hash(dgs))
    print(dgs)
    eval(repr(dgs))
    print()

    print(repr(RenderMessage()))

    render_type = RenderMessage(
        Line3D(
            RenderAnchor(),
            RenderAnchor(relative=CarAnchor(0, MyVector(1, 1, 1))),
            Color(255),
        )
    )
    if isinstance(render_type.variety.item, Line3D):
        render_type.variety.item.color.a = 150
    else:
        raise ValueError("Expected Line3D")

    print(hash(render_type))
    print(render_type)
    eval(repr(render_type))
    print()

    comm = MatchComm(3, 1, False, "Ready!", b"Hello, world!")
    print(hash(comm))
    print(comm)
    eval(repr(comm))
    print(comm.content.decode("utf-8"))
    print()

    air_state = AirState.Dodging
    print(hash(air_state))

    match air_state:
        case AirState.Dodging:
            pass
        case _:
            assert False

    try:
        AirState(8)
    except ValueError as e:
        print(e)
    print()

    invalid_data = comm.pack()

    try:
        RenderMessage.unpack(invalid_data)
    except InvalidFlatbuffer as e:
        print(e)

    match_settings = MatchSettings(
        game_path=random_string(),
        game_map_upk=random_string(),
        player_configurations=[random_player_config() for _ in range(128)],
        script_configurations=[random_script_config() for _ in range(8)],
        mutator_settings=MutatorSettings(),
    )

    data = match_settings.pack()
    print(f"MatchSettings size: {len(data)} bytes")

    renderPolyLine = RenderMessage(
        PolyLine3D(
            [Vector3() for _ in range(2048)],
            Color(255),
        ),
    )

    match renderPolyLine.variety.item:
        case PolyLine3D(points, clr):
            assert len(points) == 2048
            assert clr.a == 255
        case _:
            assert False

    data = renderPolyLine.pack()
    print(f"RenderMessage size: {len(data)} bytes")

    print()

    ballPred = BallPrediction([PredictionSlice(1) for _ in range(5 * 120)])
    data = ballPred.pack()
    print(f"BallPrediction size: {len(data)} bytes")

    print()

    print("Running quick benchmark...")

    num_trials = 60_000

    total_make_time = 0
    total_pack_time = 0
    total_unpack_time = 0
    for _ in range(num_trials):
        start = time_ns()
        desired_game_state = DesiredGameState(
            [
                DesiredBallState(
                    DesiredPhysics(
                        Vector3Partial(0, 0, 0),
                        RotatorPartial(0, 0, 0),
                        Vector3Partial(0, 0, 0),
                        Vector3Partial(0, 0, 0),
                    )
                )
                for _ in range(16)
            ],
            [
                DesiredCarState(
                    DesiredPhysics(
                        Vector3Partial(0, 0, 0),
                        RotatorPartial(0, 0, 0),
                        Vector3Partial(0, 0, 0),
                        Vector3Partial(0, 0, 0),
                    ),
                    100,
                )
                for _ in range(16)
            ],
            game_info_state=DesiredGameInfoState(
                game_speed=1, world_gravity_z=-650, end_match=True
            ),
            console_commands=[ConsoleCommand("dump_items")],
        )
        total_make_time += time_ns() - start

        start = time_ns()
        packed_bytes = desired_game_state.pack()
        total_pack_time += time_ns() - start

        start = time_ns()
        DesiredGameState.unpack(packed_bytes)
        total_unpack_time += time_ns() - start

    print(f"Average time to make: {total_make_time / num_trials / 1_000:.2f}us")
    print(f"Average time to pack: {total_pack_time / num_trials / 1_000:.2f}us")
    print(f"Average time to unpack: {total_unpack_time / num_trials / 1_000:.2f}us")

    print(f"Total time: {(total_pack_time + total_unpack_time) / 1_000_000_000:.3f}s")
