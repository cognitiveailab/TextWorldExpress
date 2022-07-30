import time
import random
import argparse

from textworld_express import TextWorldExpressEnv


def userConsole(args):
    """ Example user input console, to play through a game. """
    exitCommands = ["quit", "exit"]

    gameName = args['game_name']    

    # Initialize environment
    env = TextWorldExpressEnv(args['jar_path'], envStepLimit=args['max_steps'], threadNum=0)
    gameNames = env.getGameNames()
    print("Supported Game Names: " + str(gameNames))

    # Load the task
    gameFold = "train"
    gameSeed = args['seed']
    gameParams = ""     # e.g. "numLocations=5, includeDoors=1"    
    generateGoldPath = True
    env.load(gameName, gameFold, gameSeed, gameParams, generateGoldPath)
    
    print("Selected Game: " + str(gameName))
    print("Generation properties: " + str(env.getGenerationProperties()) )    

    if (generateGoldPath == True):
        print("Gold path: " + str(env.getGoldActionSequence()))
            
    # Initialize a random task variation in this set
    obs = env.resetWithSeed(gameSeed, gameFold, generateGoldPath)

    # Take action
    curIter = 0
    
    userInputStr = ""
    print("\nType 'exit' to quit.\n")
    while (userInputStr not in exitCommands):

        # Select a random action
        validActions = obs['validActions']        

        # Verbose output mode
        print("")
        print("Step " + str(curIter))
        print(str(obs['observation']))
        print("Score: " + str(obs['scoreRaw']) + " (raw)    " + str(obs['score']) + " (normalized)")
        print("Valid Actions: " + str(validActions))            

        if (obs['tasksuccess'] == True):
            print("Task Success!")
        if (obs['taskfailure'] == True):
            print("Task Failure!")

        # Get user input
        userInputStr = input('> ')
        # Sanitize input
        userInputStr = userInputStr.lower().strip()

        # Take action
        obs = env.step(userInputStr)

        curIter += 1

    print("Completed.")

    print("Shutting down server...")
    env.shutdown()



#
#   Parse command line arguments
#
def parse_args():
    desc = "Run a model that chooses random actions until successfully reaching the goal."
    parser = argparse.ArgumentParser(desc)
    parser.add_argument("--jar_path", type=str,
                        help="Path to the ScienceWorld jar file. Default: use builtin.")
    parser.add_argument("--game-name", type=str, choices=['cookingworld', 'coin', 'twc'], default='cookingworld',
                        help="Specify the game to play. Default: %(default)s")
    parser.add_argument("--game-fold", type=str, choices=['train', 'dev', 'test'], default='train',
                        help="Specify the game set to use (train, dev, test). Default: %(default)s")
    parser.add_argument("--max-steps", type=int, default=50,
                        help="Maximum number of steps per episode. Default: %(default)s")
    parser.add_argument("--seed", type=int, default=0,
                        help="Seed the generator for used in generating the game")

    args = parser.parse_args()
    params = vars(args)
    return params


def main():
    print("TextWorldExpress 1.0 API Examples - Human User Console")

    # Parse command line arguments
    args = parse_args()
    random.seed(args["seed"])
    userConsole(args)

if __name__ == "__main__":
    main()