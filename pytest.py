from rlbot_flatbuffers import *

if __name__ == "__main__":
    color = Color(255, 0, 0)
    print(repr(color))
    print(color)

    controller = ControllerState(throttle=1)
    controller.boost = True

    player_input = PlayerInput(0, controller)

    from time import time_ns

    num_trials = 1_000

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

    ready_message = ReadyMessage(True, wants_game_messages=True)
    print(repr(ready_message))
    print(ready_message)

    print(repr(StopCommand()))
    print(StopCommand(True))
