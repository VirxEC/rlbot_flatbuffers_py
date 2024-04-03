from time import time_ns

from rlbot_flatbuffers import *

if __name__ == "__main__":
    ready_message = ReadyMessage(True, wants_game_messages=True)
    print(repr(ready_message))
    print(ready_message)
    eval(repr(ready_message))
    print()

    dgs = DesiredGameState(
        game_info_state=DesiredGameInfoState(game_speed=Float(1), end_match=Bool())
    )
    dgs.game_info_state.world_gravity_z = Float(-650)
    dgs.game_info_state.end_match.val = True
    dgs.console_commands = [ConsoleCommand("freeze")]
    dgs.ball_state = DesiredBallState()

    print(repr(dgs))
    print(dgs)
    eval(repr(dgs))
    print()

    print(repr(RenderType()))

    render_type = RenderType(Line3D(Vector3(0, 0, 0), Vector3(1, 1, 1), Color(255)))
    if isinstance(render_type.item, Line3D):
        render_type.item.color.a = 150
    else:
        raise ValueError("Expected Line3D")

    print(repr(render_type))
    print(render_type)
    eval(repr(render_type))
    print()

    comm = MatchComm(3, 1, False, "Ready!", b"Hello, world!")
    print(repr(comm))
    print(comm)
    print(comm.content.decode("utf-8"))
    print()

    num_trials = 1_000_000

    total_make_time = 0
    total_pack_time = 0
    total_unpack_time = 0
    for _ in range(num_trials):
        start = time_ns()
        desired_game_state = DesiredGameState(
            DesiredBallState(DesiredPhysics()),
            car_states=[DesiredCarState(boost_amount=100)],
            game_info_state=DesiredGameInfoState(game_speed=1, world_gravity_z=-650, end_match=True),
            console_commands=[ConsoleCommand("freeze")],
        )
        total_make_time += time_ns() - start

        start = time_ns()
        packed_bytes = desired_game_state.pack()
        total_pack_time += time_ns() - start

        start = time_ns()
        DesiredGameState.unpack(packed_bytes)
        total_unpack_time += time_ns() - start

    print(f"Average time to make: {round(total_make_time / num_trials, 2)}ns")
    print(f"Average time to pack: {round(total_pack_time / num_trials, 2)}ns")
    print(f"Average time to unpack: {round(total_unpack_time / num_trials, 2)}ns")

    print(f"Total time: {round((total_pack_time + total_unpack_time) / 1000000, 2)}ms")
