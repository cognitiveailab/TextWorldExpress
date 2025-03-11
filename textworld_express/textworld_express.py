
import os
import sys
import time
import logging
import tempfile

try:
    import orjson  # faster json serialization
except ImportError:
    import json as orjson

from py4j.java_gateway import launch_gateway
from py4j.java_gateway import JavaGateway, GatewayParameters, CallbackServerParameters

from textworld_express.constants import BASEPATH, DEBUG_MODE, JAR_PATH

logger = logging.getLogger(__name__)


class TextWorldExpressEnv:

    #
    # Constructor
    #
    def __init__(self, serverPath=None, envStepLimit=100):
        serverPath = serverPath or JAR_PATH  # Use the builtin jar.

        # Launch the server and connect to the JVM.

        # Launch Java side with dynamic port and get back the port on which the
        # server was bound to.
        if DEBUG_MODE:
            port = launch_gateway(
                classpath=serverPath, die_on_exit=True, cwd=BASEPATH,
                javaopts=['-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005,quiet=y'],
                redirect_stdout=sys.stdout, redirect_stderr=sys.stderr)
            print("Attach debugger within the next 10 seconds")
            time.sleep(10)  # Give time for user to attach debugger
        else:
            port, proc = launch_gateway(classpath=serverPath, die_on_exit=True, cwd=BASEPATH, javaopts=['-Xverify:none'], return_proc=True)

        # Connect python side to Java side with Java dynamic port and start python
        # callback server with a dynamic port
        self._gateway = JavaGateway(
            gateway_parameters=GatewayParameters(auto_field=True, port=port),
            callback_server_parameters=CallbackServerParameters(port=0, daemonize=True),
            java_process=proc)

        # Retrieve the port on which the python callback server was bound to.
        python_port = self._gateway.get_callback_server().get_listening_port()

        # Tell the Java side to connect to the python callback server with the new
        # python port. Note that we use the java_gateway_server attribute that
        # retrieves the GatewayServer instance.
        self._gateway.java_gateway_server.resetCallbackClient(
            self._gateway.java_gateway_server.getCallbackClient().getAddress(),
            python_port)

        self.server = self._gateway.jvm.textworldexpress.runtime.PythonInterface()

        # Keep track of the last step score, to calculate reward from score
        self.lastStepScore = 0

        # Set the environment step limit
        self.envStepLimit = envStepLimit

        # Clear the run histories
        self.runHistory = []
        self.clearRunHistory()

        # By default, set that the gold path was not generated unless the user asked for it
        #self.goldPathGenerated = False

        # Cache options for environment reset.
        self.seed = None
        self.gameName = None
        self.gameParams = ""
        self.gameFold = "train"
        self.generateGoldPath = False

        self._obj_tree_tempfile = tempfile.NamedTemporaryFile()

    #
    #   Run History
    #
    def clearRunHistory(self):
        self.runHistory = []

    def getRunHistory(self):
        # Find final score
        finalScore = 0
        if (len(self.runHistory) > 0):
            finalScore = self.runHistory[-1]['score']

        # Pack history
        packed = {
            'properties': self.getGenerationProperties(),
            'finalScore': finalScore,
            'numSteps': len(self.runHistory),
            'history': self.runHistory,
        }
        return packed

    def addStepToHistory(self, step):
        self.runHistory.append(dict(step))

    def getNumSteps(self):
        return len(self.runHistory)

    #
    #   Methods
    #
    def reset(self, seed=None, gameFold=None, gameName=None, gameParams=None, generateGoldPath=False):
        self.gameName = gameName or self.gameName
        self.gameParams = gameParams if gameParams is not None else self.gameParams
        if gameName is not None or gameParams is not None:
            self.load(self.gameName, self.gameParams)

        self.gameFold = gameFold or self.gameFold
        self.seed = seed if seed is not None else self.seed

        self.goldPathGenerated = generateGoldPath
        if self.seed is None:
            response = self.server.resetWithRandomSeedJSON(self.gameFold, generateGoldPath)
        else:
            response = self.server.generateNewGameJSON(self.seed, self.gameFold, generateGoldPath)

        infos = self.parseJSONResponse(response)

        # Reset last step score (used to calculate reward from current-previous score)
        self.lastStepScore = 0

        # Add this step to a new run history
        self.clearRunHistory()
        self.addStepToHistory(infos)

        return infos["observation"], infos

    # Ask the simulator to load an environment from a script
    def load(self, gameName, gameParams):
        self.gameName = gameName
        self.gameParams = gameParams
        msg = self.server.load(self.gameName, self.gameParams)
        if msg:
            raise ValueError(msg)

    def close(self) -> None:
        self._gateway.shutdown()

        # According to https://github.com/py4j/py4j/issues/320#issuecomment-553599210
        # we need to send a newline to the process to make it exit.
        if self._gateway.java_process.poll() is None:
            self._gateway.java_process.stdin.write("\n".encode("utf-8"))
            self._gateway.java_process.stdin.flush()

    def __del__(self):
        self.close()


    # Get a list of valid tasks/environments
    def getGameNames(self):
        return list(self.server.getGameNames())

    # Get the current game's generation properties
    def getGenerationProperties(self):
        return orjson.loads(self.server.getGenerationPropertiesJSON())

    # Get the current game's task description
    def getTaskDescription(self):
        return self.server.getTaskDescription()

    def getObjectTree(self):
        msg = self.server.getObjectTree(self._obj_tree_tempfile.name)
        if msg:
            # Game is not initialized.
            raise RuntimeError(msg)

        self._obj_tree_tempfile.file.seek(0)
        payload = self._obj_tree_tempfile.file.read()
        return orjson.loads(payload)

    #
    # Train/development/test sets
    #
    def getValidSeedsTrain(self):
        return list(self.server.getSeedsTrain())

    def getValidSeedsDev(self):
        return list(self.server.getSeedsDev())

    def getValidSeedsTest(self):
        return list(self.server.getSeedsTest())

    def getRandomSeedTrain(self):
        return self.server.getRandomSeedTrain()

    def getRandomSeedDev(self):
        return self.server.getRandomSeedDev()

    def getRandomSeedTest(self):
        return self.server.getRandomSeedTest()

    #
    # Gold action sequence
    #
    def getGoldActionSequence(self):
        if (self.goldPathGenerated == True):
            return list(self.server.getGoldActionSequence())
        else:
            return ["ERROR: Gold path was not generated.  Set `generateGoldPath` flag to true when calling load()."]

    # Parse JSON (Helper)
    def parseJSONResponse(self, json_data):
        # Python built-in JSON parsing (slower)
        #parsedResponse = json.loads(json_data)
        # External JSON parser (faster)
        parsedResponse = orjson.loads(json_data)
        # Add placeholders for inferred properties
        # TODO: those should be handled server-side.
        parsedResponse['reward'] = 0
        parsedResponse['done'] = False
        parsedResponse['numMoves'] = 0

        # Also add the task description to the observation (feature request)
        parsedResponse['taskDescription'] = self.getTaskDescription()

        return parsedResponse

    #
    # Step
    #
    def step(self, inputStr:str):
        # Step 0: If `help` command, print the task description.
        if inputStr == "help":
            observation = self.getTaskDescription()
            infos = dict(self.runHistory[-1])
            return observation, 0, False, infos

        # Step 1: Take a step in the environment
        infos = self.parseJSONResponse(self.server.stepJSON(inputStr))
        observation = infos["observation"]
        infos['lastActionStr'] = inputStr

        # Step 2: Calculate reward
        score = infos['score']
        reward = score - self.lastStepScore         # Calculate reward (delta score) for this step
        self.lastStepScore = score                  # Store current score for reward calculation on the next step
        infos['reward'] = reward      # Add reward to response

        # Step 3: Calculate what move number we're currently at
        numMoves = self.getNumSteps()
        infos['numMoves'] = numMoves

        # Step 4: Calcualte whether a 'done' condition has been met.
        isCompleted = False
        # Condition 1: If the number of moves exceeds the environment step limit, then set isCompleted to be true
        if (numMoves > self.envStepLimit):
            isCompleted = True

        # Condition 2: Success if score is >= 1.0
        if (score >= 1.0):
            isCompleted = True

        # Condition 3: Done if the environment itself has set one of the done conditions
        if ((infos['tasksuccess'] == True) or (infos['taskfailure'] == True)):
            isCompleted = True

        infos['done'] = isCompleted

        # Step 5: Add this step to the run history
        self.addStepToHistory(infos)

        return observation, reward, isCompleted, infos

    def serialize(self):
        state = {
            "gameName": self.gameName,
            "gameParams": self.gameParams,
            "seed": self.seed,
            "gameFold": self.gameFold,
            "envStepLimit": self.envStepLimit,
            "generateGoldPath": self.generateGoldPath,
            "actions": [info["lastActionStr"] for info in self.runHistory[1:]],
        }
        return state

    @classmethod
    def deserialize(cls, state):
        env = cls(envStepLimit=state["envStepLimit"])
        env.reset(
            seed=state["seed"],
            gameFold=state["gameFold"],
            gameName=state["gameName"],
            gameParams=state["gameParams"],
            generateGoldPath=state["generateGoldPath"]
        )
        for action in state["actions"]:
            env.step(action)

        return env

    def clone(self):
        return self.deserialize(self.serialize())
