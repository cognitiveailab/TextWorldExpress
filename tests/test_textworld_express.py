from textworld_express import TextWorldExpressEnv


GAME_PARAMS = {
    "cookingworld": "numLocations=1, numIngredients=2, numDistractorItems=5, includeDoors=0, limitInventorySize=0",
    #"twc": "numLocations=1, numItemsToPutAway=2, includeDoors=0, limitInventorySize=0",
    "twc": "numLocations=1,numItemsToPutAway=1,includeDoors=0,limitInventorySize=0",
    "coin": "numLocations=1, numDistractorItems=5, limitInventorySize=0",
    "arithmetic": "",
    "mapreader": "numLocations=2, maxDistanceApart=1, maxDistractorItemsPerLocation=2, includeDoors=0, limitInventorySize=0",
    "sorting": "",
}


def test_observation_is_deterministic():
    env = TextWorldExpressEnv()
    # Test all games with some fixed params.
    for game_name in env.getGameNames():
        obs_orig, _ = env.reset(seed=20221120, gameFold="train", gameName=game_name, gameParams=GAME_PARAMS[game_name])

        for i in range(30):
            obs, _ = env.reset()
            assert obs == obs_orig

            obs, _, _, _ = env.step("look around")
            assert obs == obs_orig

    # Test all games with their default params.
    for game_name in env.getGameNames():
        obs_orig, _ = env.reset(seed=0, gameFold="train", gameName=game_name, gameParams="")

        for i in range(30):
            obs, _ = env.reset()
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


def test_generate_goldpath():
    env = TextWorldExpressEnv()
    # Test all games with some fixed params.
    for game_name in env.getGameNames():
        print(game_name)
        _, _ = env.reset(seed=20221120, gameFold="train", gameName=game_name, gameParams=GAME_PARAMS[game_name], generateGoldPath=True)
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
