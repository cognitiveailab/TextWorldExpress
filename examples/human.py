import sys
import random
import argparse

import textworld_express as twx
from textworld_express import TextWorldExpressEnv


prompt_toolkit_available = False
try:
    # For command line history and autocompletion.
    from prompt_toolkit import prompt
    from prompt_toolkit.completion import WordCompleter
    from prompt_toolkit.history import InMemoryHistory
    prompt_toolkit_available = sys.stdout.isatty()
except ImportError:
    pass

try:
    # For command line history when prompt_toolkit is not available.
    import readline  # noqa: F401
except ImportError:
    pass



def userConsole(args):
    """ Example user input console, to play through a game. """
    history = None
    if prompt_toolkit_available:
        history = InMemoryHistory()

    exitCommands = ["quit", "exit"]

    gameName = args['game_name']

    # Initialize environment
    env = TextWorldExpressEnv(args['jar_path'], envStepLimit=args['max_steps'])
    gameNames = env.getGameNames()
    print("Supported Game Names: " + str(gameNames))

    # Load the task
    gameFold = "train"
    gameSeed = args['seed']
    gameParams = args['game_params']  # e.g. "numLocations=5, includeDoors=1"
    generateGoldPath = True
    env.load(gameName, gameParams)

    print("Selected Game: " + str(gameName))
    print("Selected Seed: " + str(gameSeed))
    print("Generation properties: " + str(env.getGenerationProperties()) )

    # Initialize a random `gameName` game.
    obs, infos = env.reset(seed=gameSeed, gameFold=gameFold, generateGoldPath=generateGoldPath)

    # Task description
    print("Task Description: " + env.getTaskDescription())
    print("")

    if (generateGoldPath == True):
        print("Gold path: " + str(env.getGoldActionSequence()))

    # Take action
    curIter = 0

    userInputStr = ""
    print("\nType 'exit' to quit.\n")
    while (userInputStr not in exitCommands):

        # Select a random action
        validActions = infos['validActions']

        # Verbose output mode
        print("")
        print("Step " + str(curIter))
        print(str(obs))
        print("Score: " + str(infos['scoreRaw']) + " (raw)    " + str(infos['score']) + " (normalized)")
        #print("Valid Actions: " + str(validActions))

        if (infos['tasksuccess'] == True):
            print("Task Success!")
        if (infos['taskfailure'] == True):
            print("Task Failure!")

        # Get user input
        if prompt_toolkit_available:
            actions_completer = WordCompleter(validActions, ignore_case=True, sentence=True)
            userInputStr = prompt('> ', completer=actions_completer,
                                  history=history, enable_history_search=True)
        else:
            print("Valid Actions: " + str(validActions))
            userInputStr = input('> ')

        # Sanitize input
        userInputStr = userInputStr.lower().strip()

        # Take action
        obs, reward, done, infos = env.step(userInputStr)

        curIter += 1

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
    parser.add_argument("--game-params", type=str, default='',
                        help="Change game generation properties, e.g. 'numLocations=5, includeDoors=1'.")
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
