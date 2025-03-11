import time
from textworld_express import TextWorldExpressEnv


GAME_PARAMS = [
    ("cookingworld", "numLocations=1, numIngredients=2, numDistractorItems=5, includeDoors=0, limitInventorySize=0"),
    ("twc", "numLocations=1,numItemsToPutAway=1,includeDoors=0,limitInventorySize=0"),
    ("coin", "numLocations=1, numDistractorItems=5, limitInventorySize=0"),
    ("arithmetic", ""),
    ("mapreader", "numLocations=2, maxDistanceApart=1, maxDistractorItemsPerLocation=2, includeDoors=0, limitInventorySize=0"),
    ("sorting", ""),
    ("simonsays", "gameLength=2, numDistractors=1, memorization=0"),
    ("simonsays", "gameLength=6, numDistractors=4, memorization=1"),
    ("simonsays", "gameLength=1000, numDistractors=4, memorization=0"),
    ("simonsays", "gameLength=1000, numDistractors=4, memorization=1"),
    ("peckingorder", ""),
]


def test_observation_is_deterministic():
    env = TextWorldExpressEnv()
    # Test all games with their default params.
    for game_name in env.getGameNames():
        obs_orig, _ = env.reset(seed=0, gameFold="train", gameName=game_name, gameParams="")

        for i in range(30):
            obs, _ = env.reset()
            assert obs == obs_orig

            if game_name not in ["simonsays"]:
                obs, _, _, _ = env.step("look around")
                assert obs == obs_orig

    # Test all games with some fixed params.
    for game_name, game_params in GAME_PARAMS:
        obs_orig, _ = env.reset(seed=20221120, gameFold="train", gameName=game_name, gameParams=game_params)

        for i in range(30):
            obs, _ = env.reset()
            assert obs == obs_orig

            if game_name not in ["simonsays"]:
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


def test_generate_goldpath():
    env = TextWorldExpressEnv()
    # Test all games with some fixed params.
    for game_name, game_params in GAME_PARAMS:
        print(game_name)
        _, _ = env.reset(seed=20221120, gameFold="train", gameName=game_name, gameParams=game_params, generateGoldPath=True)
        print(len(env.getGoldActionSequence()))
        print(env.getGoldActionSequence())


def test_object_tree_cookingworld():
    env = TextWorldExpressEnv()
    obs, infos = env.reset(gameName="cookingworld", seed=20221120, gameFold="train", generateGoldPath=True)

    obj_tree = env.getObjectTree()

    expected_location_names = ['backyard', 'bathroom', 'bedroom', 'corridor', 'driveway', 'kitchen', 'laundry room', 'living room', 'pantry', 'street', 'supermarket']
    assert sorted(obj_tree["locations"].keys()) == expected_location_names
    assert obj_tree["player_location"] == "kitchen"
    assert sorted(obj_tree["inventory"].keys()) == []
    assert sorted(obj_tree["deleted_objects"].keys()) == []

    # Follow walkthrough
    for cmd in env.getGoldActionSequence():
        env.step(cmd)

    obj_tree_end = env.getObjectTree()
    assert sorted(obj_tree_end["deleted_objects"].keys()) == ["meal"]
    assert sorted(obj_tree_end["inventory"].keys()) == ["cookbook", "knife"]

    def _extract_contents(root):
        if "contents" not in root:
            return {}

        objs = dict(root["contents"])
        for node in root["contents"].values():
            objs.update(_extract_contents(node))

        return objs

    # check objects in the kitchen
    expected_objs_in_kitchen = ['counter', 'cutlery drawer', 'dining chair', 'dining table', 'dishwasher', 'fridge', 'oven', 'stove', 'trash can']
    assert sorted(obj_tree["locations"]["kitchen"]["contents"].keys()) == expected_objs_in_kitchen

    # collect all objects in the kitchen (including nested ones)
    objs = _extract_contents(obj_tree["locations"]["kitchen"])
    expected_nested_objs_in_kitchen = ['cookbook', 'counter', 'cutlery drawer', 'dining chair', 'dining table', 'dishwasher', 'fridge', 'green bell pepper', 'knife', 'oven', 'stove', 'trash can']
    assert sorted(objs.keys()) == expected_nested_objs_in_kitchen


def test_reset_with_seed():
    env = TextWorldExpressEnv()
    obs1, _ = env.reset(seed=42, gameName="cookingworld")
    obs2, _ = env.reset(seed=42, gameName="cookingworld")
    assert obs1 == obs2


def test_reset_without_seed():
    env = TextWorldExpressEnv()
    obs1, _ = env.reset(gameName="cookingworld")
    obs2, _ = env.reset(gameName="cookingworld")
    assert obs1 != obs2


def test_step():
    env = TextWorldExpressEnv()
    env.reset(gameName="cookingworld")
    obs, reward, done, infos = env.step("look around")
    assert isinstance(obs, str)
    assert isinstance(reward, float)
    assert isinstance(done, bool)
    assert isinstance(infos, dict)


def test_clone():
    env = TextWorldExpressEnv()
    infos = env.reset(gameName="cookingworld", seed=42, generateGoldPath=True)
    env.step("look around")
    solution = env.getGoldActionSequence()
    for action in solution[:len(solution) // 2]:
        env.step(action)

    clone_env = env.clone()
    assert env.getRunHistory() == clone_env.getRunHistory()

    # Continue the game in both env.
    for action in solution[len(solution) // 2:]:
        env.step(action)
        clone_env.step(action)

    assert env.getRunHistory() == clone_env.getRunHistory()


def test_serialize_deserialize():
    env = TextWorldExpressEnv()
    env.reset(gameName="cookingworld", seed=42)
    env.step("look around")
    state = env.serialize()
    new_env = TextWorldExpressEnv.deserialize(state)
    assert env.getRunHistory() == new_env.getRunHistory()


def test_close():
    env = TextWorldExpressEnv()
    env.reset(gameName="cookingworld")
    assert env._gateway.java_process.poll() is None
    env.close()
    time.sleep(1)
    assert env._gateway.java_process.poll() is not None


def test_get_game_names():
    env = TextWorldExpressEnv()
    game_names = env.getGameNames()
    assert isinstance(game_names, list)
    assert "cookingworld" in game_names
