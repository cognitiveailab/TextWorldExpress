from textworld_express import TextWorldExpressEnv


def test_observation_is_deterministic():
    env = TextWorldExpressEnv()
    for game_name in env.getGameNames():
        if game_name in ("arithmetic",): continue  # TODO: check why those games are not reproducible.

        #env.load(game_name, gameFold="train", seed=20221120, paramStr="")
        obs_orig, _ = env.reset(seed=20221120, gameFold="train", gameName=game_name)#, paramStr="")

        for i in range(30):
            obs, _ = env.reset()#seed=20221120, gameFold="train")
            assert obs == obs_orig

            obs, _, _, _ = env.step("look around")
            assert obs == obs_orig


def test_multiple_instances():
    env1 = TextWorldExpressEnv()
    env2 = TextWorldExpressEnv()

    assert env1._gateway._gateway_client.port != env2._gateway._gateway_client.port

    obs1, _ = env1.reset(gameName="cookingworld", seed=20221120, gameFold="train")
    obs2, _ = env2.reset(gameName="cookingworld", seed=20221120, gameFold="train")

    # Check if the two observations are the same when ignoring the order in which objects are described.
    assert obs1 == obs2

    # Interact with one of the envs.
    env1.step("open fridge")

    # Check if the observations now differ from each other.
    obs1_1, _, _, _ = env1.step("look around")
    obs2_1, _, _, _ = env2.step("look around")
    assert obs1_1 != obs2_1

    # Resetting the second env doesn't affect the first one.
    env2.reset()

    obs1_2, _, _, _ = env1.step("look around")
    assert obs1_1 == obs1_2

    env2.step("open fridge")
    obs2_2, _, _, _ = env2.step("look around")
    assert obs1_2 == obs2_2