import textwrap
import argparse
from time import time

import numpy as np
from tqdm import tqdm
from termcolor import colored
import jericho


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("filenames", nargs="+",
                        help="Path to a Z-Machine game(s).")
    parser.add_argument("--get-valid-action", action="store_true",
                        help="Get valid actions at each time step.")
    parser.add_argument("--limit", type=int,
                        help="Limit the number of steps per game. Default: no limit.")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Print the last observation when not achieving max score.")
    return parser.parse_args()

args = parse_args()

speeds = []
filename_max_length = max(map(len, args.filenames))
for filename in sorted(args.filenames):

    env = jericho.FrotzEnv(filename)
    if not env.is_fully_supported:
        print(colored("SKIP\tUnsupported game", 'yellow'))
        continue

    if "walkthrough" not in env.bindings:
        print(colored("SKIP\tMissing walkthrough", 'yellow'))
        continue

    env.reset()

    #walkthrough = bindings['walkthrough'].split('/')
    start = time()
    done = False
    for no_step, cmd in enumerate(tqdm(env.get_walkthrough())):
        if no_step == args.limit:
            break
        if args.get_valid_action:
            _ = env.get_valid_actions()
        obs, rew, done, info = env.step(cmd)

    print(filename.ljust(filename_max_length), end=" ")
    if not done:
        print(colored("FAIL", 'red'))
    elif info["score"] != env.get_max_score():
        msg = "FAIL\tDone but score {}/{}".format(info["score"], env.get_max_score())
        print(colored(msg, 'red'))
        if args.verbose:
            print(textwrap.indent(obs, prefix="  "))

    else:
        print(colored("PASS", 'green'))

    duration = time() - start
    speed = env.get_moves() / duration
    speeds.append(speed)

print("-----\nAverage: {:,.1f} steps/sec".format(np.mean(speeds)))
