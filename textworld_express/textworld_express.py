from py4j.java_gateway import JavaGateway, GatewayParameters
import subprocess

import os
import time
import orjson  # faster json serialization

import textworld_express

BASEPATH = os.path.dirname(os.path.abspath(__file__))
JAR_FILE = 'textworld-express-{version}.jar'.format(version=textworld_express.__version__)
JAR_PATH = os.path.join(BASEPATH, JAR_FILE)


class TextWorldExpressEnv:

    #
    # Constructor
    #
    def __init__(self, serverPath=None, envStepLimit=100, threadNum=0, launchServer=True):
        serverPath = serverPath or JAR_PATH  # Use the builtin jar.

        # Define the port number
        self.portNum = 25335 + threadNum

        # Launch the server
        if (launchServer == True):
            self.launchServer(serverPath)

        # Connect to the JVM
        self.gateway = JavaGateway(gateway_parameters=GatewayParameters(auto_field=True, port=self.portNum))

        # Keep track of the last step score, to calculate reward from score
        self.lastStepScore = 0

        # Load the script
        #self.load(self.gameName, 0, "")

        # Set the environment step limit
        self.envStepLimit = envStepLimit

        # Clear the run histories
        #self.clearRunHistories()

        # By default, set that the gold path was not generated unless the user asked for it
        #self.goldPathGenerated = False

        # Most recent response
        self.responseStr = ""
        self.parsedResponse = {}

    #
    #   Destructor
    #
    def __del__(self):
        # Shutdown the server
        self.shutdown()



    #
    #   Methods
    #

    # Launches the PY4J server
    def launchServer(self, serverPath):
        print("Launching TextWorldExpress Server (Port " + str(self.portNum) + ") -- this may take a moment.")
        cmd = "nohup java -cp " + serverPath + " textworldexpress.runtime.PythonInterface " + str(self.portNum) + " >/dev/null 2>&1 &"

        subprocess.Popen(cmd, cwd=BASEPATH, shell=True)
        # The sleep command here is to give time for the server process to spawn.
        # If you are spawning many threads simultaneously, you may need to increase this time.
        time.sleep(5)

    # Ask the simulator to load an environment from a script
    def load(self, gameName, gameFold, seed, paramStr, generateGoldPath=False):
        #print("Load: " + gameName + " (seed: " + str(seed) + ", gameFold: " + str(gameFold) + ")")

        self.responseStr = self.gateway.loadJSON(gameName, gameFold, seed, paramStr, generateGoldPath)
        self.parseJSONResponse()

        # Reset last step score (used to calculate reward from current-previous score)
        self.lastStepScore = 0

        # Keep track of whether the gold path was generated, to generate verbose error messages
        self.goldPathGenerated = generateGoldPath

        return self.parsedResponse

#    # Test to see if the storage class can be directly loaded
#    def loadTEST(self, gameName, gameFold, seed, paramStr, generateGoldPath=False):
#        #print("Load: " + gameName + " (seed: " + str(seed) + ", gameFold: " + str(gameFold) + ")")
#
#        return self.gateway.load(gameName, gameFold, seed, paramStr, generateGoldPath)

    # Ask the simulator to reset an environment back to it's initial state
    def resetWithSeed(self, seed, gameFold, generateGoldPath=False):
        self.responseStr = self.gateway.generateNewGameJSON(seed, gameFold, generateGoldPath)
        self.parseJSONResponse()

        # Reset last step score (used to calculate reward from current-previous score)
        self.lastStepScore = 0

        return self.parsedResponse

    # Ask the simulator to reset an environment back to it's initial state
    def resetWithRandomSeed(self, gameFold, generateGoldPath=False):
        self.responseStr = self.gateway.resetWithRandomSeedJSON(gameFold, generateGoldPath)
        self.parseJSONResponse()

        # Reset last step score (used to calculate reward from current-previous score)
        self.lastStepScore = 0

        return self.parsedResponse


    # Shutdown the scala server
    def shutdown(self):
        if hasattr(self, 'gateway'):
            self.gateway.shutdown()


    # Get a list of valid tasks/environments
    def getGameNames(self):
        return list(self.gateway.getGameNames())

    # Get the current game's generation properties
    def getGenerationProperties(self):        
        return orjson.loads(self.gateway.getGenerationPropertiesJSON())

    #
    # Train/development/test sets
    #
    def getValidSeedsTrain(self):
        return list(self.gateway.getSeedsTrain())

    def getValidSeedsDev(self):
        return list(self.gateway.getSeedsDev())

    def getValidSeedsTest(self):
        return list(self.gateway.getSeedsTest())

    def getRandomSeedTrain(self):
        return self.gateway.getRandomSeedTrain()

    def getRandomSeedDev(self):
        return self.gateway.getRandomSeedDev()

    def getRandomSeedTest(self):
        return self.gateway.getRandomSeedTest()

    #
    # Gold action sequence
    #
    def getGoldActionSequence(self):
        if (self.goldPathGenerated == True):
            return list(self.gateway.getGoldActionSequence())
        else:
            return ["ERROR: Gold path was not generated.  Set `generateGoldPath` flag to true when calling load()."]

    # Parse JSON (Helper)
    def parseJSONResponse(self):
        # Python built-in JSON parsing (slower)
        #self.parsedResponse = json.loads(self.responseStr)
        # External JSON parser (faster)
        self.parsedResponse = orjson.loads(self.responseStr)


    #
    # Step
    #
    def step(self, inputStr:str):
        # Take a step in the environment
        self.responseStr = self.gateway.stepJSON(inputStr)
        self.parseJSONResponse()

        # Calculate reward
        score = self.parsedResponse['score']
        reward = score - self.lastStepScore         # Calculate reward (delta score) for this step
        self.lastStepScore = score                  # Store current score for reward calculation on the next step
        self.parsedResponse['reward'] = reward      # Add reward to response


        # If the number of moves exceeds the environment step limit, then set isCompleted to be true
        #if (numMoves > self.envStepLimit):
        #    isCompleted = True

        return self.parsedResponse

