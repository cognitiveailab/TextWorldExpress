import time
import random
import argparse

import textworld_express as twx
from textworld_express import TextWorldExpressEnv


totalTime = 0
totalSteps = 0

def randomModel(args):
    global totalTime, totalSteps
    """ Example random agent -- randomly picks an action at each step. """
    exitCommands = ["quit", "exit"]

    gameName = args['game_name']
    numEpisodes = args['num_episodes']

    # Keep track of the agent's final scores
    finalScores = []


    # Initialize environment
    env = TextWorldExpressEnv(args['jar_path'], envStepLimit=args['max_steps'])
    gameNames = env.getGameNames()
    print("Supported Game Names: " + str(gameNames))

    # Load the task
    gameFold = "train"
    gameSeed = 0
    gameParams = ""     # e.g. "numLocations=5, includeDoors=1"
    generateGoldPath = args['gold_paths']
    env.load(gameName=gameName, gameParams=gameParams)


    time.sleep(2)

    print("Starting to run " + str(numEpisodes) + " episodes...")

    # Start running episodes
    for episodeIdx in range(0, numEpisodes):
        startTime = time.process_time()

        if (args['verbose'] == True):
            print("")
            print("Episode " + str(episodeIdx))
            print("Generation properties: " + str(env.getGenerationProperties()) )
            if (generateGoldPath == True):
                print("Gold path: " + str(env.getGoldActionSequence()))

        # Initialize a random task variation in this set
        obs, infos = env.reset(gameFold=gameFold, generateGoldPath=generateGoldPath)

        # Take action
        curIter = 0

        for stepIdx in range(0, args['max_steps']):

            # Select a random action
            validActions = infos['validActions']
            randomAction = random.choice(validActions)

            # Verbose output mode
            if (args['verbose'] == True):
                print("Step " + str(stepIdx))
                print("Observation: " + str(obs))
                print("Next random action: " + str(randomAction))

            # Take action
            obs, _, _, infos = env.step(randomAction)

            curIter += 1


        # Keep track of timing
        deltaTime = time.process_time() - startTime
        totalTime += deltaTime
        totalSteps += curIter

        if (args['verbose'] == True):
            print("History:")
            print(env.getRunHistory())

    print("Completed.")



#
#   Parse command line arguments
#
def parse_args():
    desc = "Run a model that chooses random actions until successfully reaching the goal."
    parser = argparse.ArgumentParser(desc)
    parser.add_argument("--jar_path", type=str,
                        help="Path to the TextWorldExpress jar file. Default: use builtin.")
    parser.add_argument("--game-name", type=str, choices=twx.GAME_NAMES, default=twx.GAME_NAMES[0],
                        help="Specify the game to play. Default: %(default)s")
    parser.add_argument("--game-fold", type=str, choices=['train', 'dev', 'test'], default='train',
                        help="Specify the game set to use (train, dev, test). Default: %(default)s")
    parser.add_argument("--max-steps", type=int, default=50,
                        help="Maximum number of steps per episode. Default: %(default)s")
    parser.add_argument("--num-episodes", type=int, default=100,
                        help="Number of episodes to play. Default: %(default)s")
    parser.add_argument("--seed", type=int,
                        help="Seed the random generator used for sampling random actions.")
    #parser.add_argument("--game-params", type=str, default="",
    #                    help="Specify game parameters in a comma-delmited list, e.g. 'numLocations=5, includeDoors=1'.")
    parser.add_argument("--gold-paths", action='store_true', help="Generate gold paths for each game episode.")
    parser.set_defaults(gold_paths=False)
    parser.add_argument("--verbose", action='store_true', help="Verbose output.")
    parser.set_defaults(verbose=False)

#    parser.add_argument("--output-path-prefix", default="save-histories",
#                        help="Path prefix to use for saving episode transcripts. Default: %(default)s")
#    parser.add_argument("--max-episode-per-file", type=int, default=1000,
#                        help="Maximum number of epsiodes per transcript file. Default: %(default)s")

    args = parser.parse_args()
    params = vars(args)
    return params


def main():
    print("TextWorldExpress 1.0 API Examples - Random Agent")

    # Parse command line arguments
    args = parse_args()
    random.seed(args["seed"])
    randomModel(args)

    rate = totalSteps / totalTime

    print("")
    print("----------------------------------------------------")
    print(" Performance Summary")
    print("----------------------------------------------------")
    print("Total episodes    : " + str(args['num_episodes']))
    print("Total steps       : " + str(totalSteps))
    print("Total time        : " + str(totalTime) + " seconds")
    print("Rate              : " + str(rate) + " steps per second")

if __name__ == "__main__":
    main()
