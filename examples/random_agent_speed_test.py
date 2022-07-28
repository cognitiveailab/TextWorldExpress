import time
import random
import argparse

from textworld_express import TextWorldExpressEnv as twx

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
    env = twx(args['jar_path'], envStepLimit = args['max_steps'] , threadNum = 0)
    gameNames = env.getGameNames()
    print("Supported Game Names: " + str(gameNames))

    # Load the task
    gameFold = "train"
    gameSeed = 0
    gameParams = ""
    generateGoldPath = False
    env.load(gameName, gameFold, gameSeed, gameParams, generateGoldPath) 
    

    time.sleep(2)    

    # Start running episodes
    for episodeIdx in range(0, numEpisodes):
        startTime = time.process_time()

        # Initialize a random task variation in this set        
        obs = env.resetWithRandomSeed(gameFold, generateGoldPath)

    
        # Take action
        curIter = 0

        for stepIdx in range(0, args['max_steps']):
            #print("Step " + str(stepIdx))
            #print("Observation: " + str(obs))

            # Select a random action
            validActions = obs['validActions']
            randomAction = random.choice( validActions )
            #print("Next random action: " + str(randomAction))
            
            # Take action
            obs = env.step(randomAction)

            curIter += 1
        

        # Keep track of timing
        deltaTime = time.process_time() - startTime
        totalTime += deltaTime
        totalSteps += curIter

        #time.sleep(1)


    print("Shutting down server...")
    env.shutdown()

    print("Completed.")


#
#   Parse command line arguments
#
def parse_args():
    desc = "Run a model that chooses random actions until successfully reaching the goal."
    parser = argparse.ArgumentParser(desc)
    parser.add_argument("--jar_path", type=str,
                        help="Path to the ScienceWorld jar file. Default: use builtin.")
    parser.add_argument("--game-name", type=str, choices=['cooking', 'coin', 'twc'], default='cooking',
                        help="Specify the game to play. Default: %(default)s")
    parser.add_argument("--game-fold", type=str, choices=['train', 'dev', 'test'], default='train',
                        help="Specify the game set to use (train, dev, test). Default: %(default)s")
    parser.add_argument("--max-steps", type=int, default=50,
                        help="Maximum number of steps per episode. Default: %(default)s")
    parser.add_argument("--num-episodes", type=int, default=100,
                        help="Number of episodes to play. Default: %(default)s")
    parser.add_argument("--seed", type=int,
                        help="Seed the random generator used for sampling random actions.")
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

    print("Total time: " + str(totalTime))
    print("Total steps: " + str(totalSteps))
    rate = totalSteps / totalTime
    print("Rate: " + str(rate) + " steps per second")

if __name__ == "__main__":
    main()
