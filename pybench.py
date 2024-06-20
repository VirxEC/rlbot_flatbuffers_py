from time import time_ns

import rlbot_flatbuffers as flat


def test_gtp():
    times = []

    gtp = flat.GameTickPacket(
        ball=flat.BallInfo(
            shape=flat.CollisionShape(flat.SphereShape()),
        ),
        players=[flat.PlayerInfo() for _ in range(128)],
        boost_pad_states=[flat.BoostPadState() for _ in range(128)],
        teams=[flat.TeamInfo() for _ in range(2)],
    )

    for _ in range(20_000):
        start = time_ns()

        packed = gtp.pack()
        flat.GameTickPacket.unpack(packed)

        times.append(time_ns() - start)

    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")


def test_ballpred():
    times = []

    ballPred = flat.BallPrediction([flat.PredictionSlice() for _ in range(720)])

    for _ in range(10_000):
        start = time_ns()

        ballpred_bytes = ballPred.pack()
        flat.BallPrediction.unpack(ballpred_bytes)

        times.append(time_ns() - start)

    print(f"Total time: {sum(times) / 1_000_000_000:.3f}s")
    avg_time_ns = sum(times) / len(times)
    print(f"Average time per: {avg_time_ns / 1000:.1f}us")


if __name__ == "__main__":
    test_gtp()
    print()
    test_ballpred()
