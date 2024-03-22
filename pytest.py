from time import time_ns

from rlbot_flatbuffers import *

if __name__ == "__main__":
    color = Color(255, 0, 0)
    print(repr(color))
    print(color)
    print()

    controller = ControllerState(throttle=1)
    controller.boost = True

    player_input = PlayerInput(0, controller)

    ready_message = ReadyMessage(True, wants_game_messages=True)
    print(repr(ready_message))
    print(ready_message)
    print()

    print(repr(StopCommand()))
    print(StopCommand(True))
    print()

    dgs = DesiredGameState(game_info_state=DesiredGameInfoState(game_speed=Float(1), end_match=Bool()))
    dgs.game_info_state.world_gravity_z = Float(-650)
    dgs.game_info_state.end_match.val = True

    print(repr(dgs))
    print(dgs)
    print()

    render_type = RenderType(Line3D(Vector3(0, 0, 0), Vector3(1, 1, 1), Color(255)))
    render_type.line_3_d.color.a = 150

    print(repr(RenderType()))

    print(repr(render_type))
    print(render_type)
    print()

    num_trials = 1_000_000

    total_pack_time = 0
    total_unpack_time = 0
    for _ in range(num_trials):
        start = time_ns()
        packed_bytes = player_input.pack()
        total_pack_time += time_ns() - start

        start = time_ns()
        PlayerInput.unpack(packed_bytes)
        total_unpack_time += time_ns() - start

    print(f"Average time to pack: {total_pack_time / num_trials} ns")
    print(f"Average time to unpack: {total_unpack_time / num_trials} ns")

    print(f"Total time: {(total_pack_time + total_unpack_time) / 1000000 }ms")
