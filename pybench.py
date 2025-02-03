from threading import Thread
from time import time_ns

import rlbot_flatbuffers as flat


def test_gtp():
    print("Testing GamePacket")

    pack_times = []
    unpack_times = []

    gtp = flat.GamePacket(
        balls=[flat.BallInfo(shape=flat.SphereShape()) for _ in range(128)],
        players=[flat.PlayerInfo() for _ in range(128)],
        boost_pads=[flat.BoostPadState() for _ in range(128)],
        teams=[flat.TeamInfo() for _ in range(2)],
    )

    print(len(gtp.pack()))

    for _ in range(30_000):
        start = time_ns()
        packed = gtp.pack()
        pack_times.append(time_ns() - start)

        start = time_ns()
        flat.GamePacket.unpack(packed)
        unpack_times.append(time_ns() - start)

    print(f"Total time: {(sum(pack_times) + sum(unpack_times)) / 1_000_000_000:.3f}s")

    avg_time_ns = sum(pack_times) / len(pack_times)
    print(f"Average pack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum pack time per: {min(pack_times) / 1000:.1f}us")

    avg_time_ns = sum(unpack_times) / len(unpack_times)
    print(f"Average unpack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum unpack time per: {min(unpack_times) / 1000:.1f}us")


def test_ballpred():
    print("Testing 10s BallPrediction")

    pack_times = []
    unpack_times = []

    ballPred = flat.BallPrediction([flat.PredictionSlice(1) for _ in range(120 * 10)])

    print(len(ballPred.pack()))

    for _ in range(10_000):
        start = time_ns()
        packed = ballPred.pack()
        pack_times.append(time_ns() - start)

        start = time_ns()
        flat.BallPrediction.unpack(packed)
        unpack_times.append(time_ns() - start)

    print(f"Total time: {(sum(pack_times) + sum(unpack_times)) / 1_000_000_000:.3f}s")

    avg_time_ns = sum(pack_times) / len(pack_times)
    print(f"Average pack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum pack time per: {min(pack_times) / 1000:.1f}us")

    avg_time_ns = sum(unpack_times) / len(unpack_times)
    print(f"Average unpack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum unpack time per: {min(unpack_times) / 1000:.1f}us")

    print()
    print("Testing 6s BallPrediction")

    pack_times = []
    unpack_times = []

    ballPred = flat.BallPrediction([flat.PredictionSlice(1) for _ in range(120 * 6)])

    print(len(ballPred.pack()))

    for _ in range(10_000):
        start = time_ns()
        packed = ballPred.pack()
        pack_times.append(time_ns() - start)

        start = time_ns()
        flat.BallPrediction.unpack(packed)
        unpack_times.append(time_ns() - start)

    print(f"Total time: {(sum(pack_times) + sum(unpack_times)) / 1_000_000_000:.3f}s")

    avg_time_ns = sum(pack_times) / len(pack_times)
    print(f"Average pack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum pack time per: {min(pack_times) / 1000:.1f}us")

    avg_time_ns = sum(unpack_times) / len(unpack_times)
    print(f"Average unpack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum unpack time per: {min(unpack_times) / 1000:.1f}us")


def find_slice_at_time(ball_prediction: flat.BallPrediction, game_time: float):
    """
    This will find the future position of the ball at the specified time. The returned
    Slice object will also include the ball's velocity, etc.
    """
    start_time = ball_prediction.slices[0].game_seconds
    approx_index = int(
        (game_time - start_time) * 120
    )  # We know that there are 120 slices per second.
    if 0 <= approx_index < len(ball_prediction.slices):
        return ball_prediction.slices[approx_index]
    return None


def test_loop():
    print("Testing access times")

    ballPred = flat.BallPrediction([flat.PredictionSlice(1) for _ in range(120 * 6)])

    start = time_ns()
    for _ in range(100):

        li = []
        for t in range(1, 301):
            ball_in_future = find_slice_at_time(ballPred, t / 60)
            li.append(ball_in_future)
    print(f"Total time: {(time_ns() - start) / 1_000_000:.3f}ms")

    times = []
    for _ in range(50_000):
        start = time_ns()

        li = []
        for t in range(1, 301):
            ball_in_future = find_slice_at_time(ballPred, t / 60)
            li.append(ball_in_future)

        times.append(time_ns() - start)

    print()
    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum time per: {min(times) / 1000:.1f}us")

    times = []
    for _ in range(50_000):
        start = time_ns()

        li = [find_slice_at_time(ballPred, t / 60) for t in range(1, 301)]

        times.append(time_ns() - start)

    print()
    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum time per: {min(times) / 1000:.1f}us")

    times = []
    for _ in range(1_000_000):
        start = time_ns()

        li = list(ballPred.slices[1:602:2])

        times.append(time_ns() - start)

    print()
    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum time per: {min(times) / 1000:.1f}us")


def test_renders():
    print("Testing rendering")

    times = []

    for _ in range(20_000):
        start = time_ns()

        renders = [
            flat.RenderMessage(
                flat.Line3D(
                    flat.RenderAnchor(world=flat.Vector3(0, 0, 0)),
                    flat.RenderAnchor(
                        relative=flat.CarAnchor(0, flat.Vector3(1, 1, 1))
                    ),
                    flat.Color(255, a=150),
                )
            ),
            flat.RenderMessage(
                flat.Line3D(
                    flat.RenderAnchor(world=flat.Vector3(0, 0, 0)),
                    flat.RenderAnchor(
                        relative=flat.CarAnchor(0, flat.Vector3(1, 1, 1))
                    ),
                    flat.Color(255, a=150),
                )
            ),
            flat.RenderMessage(
                flat.Line3D(
                    flat.RenderAnchor(world=flat.Vector3(0, 0, 0)),
                    flat.RenderAnchor(
                        relative=flat.CarAnchor(0, flat.Vector3(1, 1, 1))
                    ),
                    flat.Color(255, a=150),
                )
            ),
            flat.RenderMessage(
                flat.String3D(
                    "Hello, world!", flat.RenderAnchor(world=flat.Vector3(0, 0, 0)), 1
                )
            ),
        ]

        render_group = flat.RenderGroup(renders)
        render_group.pack()

        times.append(time_ns() - start)

    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum time per: {min(times) / 1000:.1f}us")


def test_threaded(func):
    threads = []

    for _ in range(4):
        t = Thread(target=func)
        t.start()
        threads.append(t)
    
    for t in threads:
        t.join()


if __name__ == "__main__":
    test_gtp()
    print()
    test_ballpred()
    print()
    test_loop()
    print()
    test_renders()

    # print("testing functions for thread safety")

    # funcs = [test_gtp, test_ballpred, test_loop, test_renders]

    # for func in funcs:
    #     test_threaded(func)
    #     print()
