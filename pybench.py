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

    for _ in range(20_000):
        start = time_ns()
        packed = gtp.pack()
        pack_times.append(time_ns() - start)

        start = time_ns()
        flat.GamePacket.unpack(packed)
        unpack_times.append(time_ns() - start)

    avg_time_ns = sum(pack_times) / len(pack_times)
    print(f"Average pack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum pack time per: {min(pack_times) / 1000:.1f}us")

    avg_time_ns = sum(unpack_times) / len(unpack_times)
    print(f"Average unpack time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum unpack time per: {min(unpack_times) / 1000:.1f}us")


def test_ballpred():
    print("Testing BallPrediction")

    times = []

    ballPred = flat.BallPrediction([flat.PredictionSlice(1) for _ in range(960)])

    for _ in range(100_000):
        start = time_ns()

        packed = ballPred.pack()
        flat.BallPrediction.unpack(packed)

        times.append(time_ns() - start)

    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")
    print(f"Minimum time per: {min(times) / 1000:.1f}us")


if __name__ == "__main__":
    test_gtp()
    print()
    test_ballpred()
