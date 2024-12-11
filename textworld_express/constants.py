import os

BASEPATH = os.path.dirname(os.path.abspath(__file__))
JAR_FILE = 'textworld-express.jar'
JAR_PATH = os.path.join(BASEPATH, JAR_FILE)


GAME_NAMES = ['cookingworld', 'coin', 'twc', 'mapreader', 'arithmetic', 'sorting', 'simonsays', 'peckingorder']


def is_in_debug_mode() -> bool:
    """Determine whether debug mode should be enabled.

    Debug mode sends JAR output to the console and allows the user to attach a debugger at port 5005.

    To enable debug mode, set the environment variable TWX_DEBUG to "1" or "true".
    """
    if "TWX_DEBUG" not in os.environ:
        return False
    env_value = os.environ["TWX_DEBUG"].lower()
    if env_value in {"1", "true"}:
        return True
    elif env_value in {"", "0", "false"}:
        return False
    else:
        raise ValueError(f'{env_value!r} is not a valid value for "TWX_DEBUG"')


DEBUG_MODE = is_in_debug_mode()
