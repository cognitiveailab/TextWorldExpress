# Precrawled path reader

import random
import json
import time


class PrecrawledPathReader:

    def __init__(self, filenameJSON):
        self.precrawledPath = PrecrawledPathReader.loadPrecrawledPath(filenameJSON)

        self.nodeLUT = self.precrawledPath['nodeLUT']
        self.stringLUT = self.precrawledPath['stringLUT']

        self.currentNodeIdx = 0
        self.currentNode = self.nodeLUT[self.currentNodeIdx]
        self.currentNodeUnpacked = self.__unpackNode()


    # Load a precrawled path from JSON
    def loadPrecrawledPath(filenameJSON):    
        with open(filenameJSON, 'r') as f:
            precrawledPath = json.load(f)
        return precrawledPath

    # Reset to the beginning state
    def reset(self):
        self.currentNodeIdx = 0
        self.currentNode = self.nodeLUT[self.currentNodeIdx]
        self.currentNodeUnpacked = self.__unpackNode()

        return self.currentNodeUnpacked


    # Convert the current hashed observation to (largely) human-readable strings
    def __unpackNode(self):
        curResult = self.currentNode['result']
        curSteps = self.currentNode['steps']

        result = {'obs': self.stringLUT[curResult['obs']],
                  'look': self.stringLUT[curResult['look']],
                  'inv': self.stringLUT[curResult['inv']],
                  'acts': [self.stringLUT[x] for x in curResult['acts']],
                  'score': curResult['score'],
                  'scoreNorm': curResult['scoreNorm'],
                  'succ': curResult['succ'],
                  'fail': curResult['fail'], 
                  'valid': curResult['valid'] 
                  }
        
        steps = {}
        for step in curSteps.items():
            actionStr = self.stringLUT[int(step[0])]
            #steps[actionStr] = self.nodeLUT[step[1]]
            steps[actionStr] = step[1]
            
        packed = {'result': result, 'steps': steps}
        return packed


    # Step
    def step(self, actionStr):        
        # Find the node index for the action
        self.currentNodeIdx = self.currentNodeUnpacked['steps'][actionStr]
        self.currentNode = self.nodeLUT[self.currentNodeIdx]
        self.currentNodeUnpacked = self.__unpackNode()

        return self.currentNodeUnpacked



#
#   Main
#

filenameIn = "precrawledpaths/precrawled-gamecoin-var0-foldtrain-maxDepth10-includeDoors0-limitInventorySize0-numDistractorItems0-numLocations4.json"
numSteps = 50
numEnvsToRun = 10000

totalStepsRun = 0

print("Loading JSON (" + filenameIn + ")")
path = PrecrawledPathReader(filenameIn)

startTime = time.process_time()
# Run the environment N times
for envIdx in range(0, numEnvsToRun):
    #print("Environment " + str(envIdx))

    # Starting node
    curNode = path.reset()
    for stepIdx in range(0, numSteps):
        # NOTE: path.currentNodeUnpacked is roughly equivalent to the 'obs' returned from the environment using the normal online API
        #print(path.currentNodeUnpacked)
        
        # Check for end of path
        if (len(curNode['steps']) == 0):
            break

        # Randomly pick one key
        curActions = curNode['result']['acts']
        curAction = random.choice(curActions)

        # Step
        curNode = path.step(curAction)
        
        # Keep track of the number of steps
        totalStepsRun += 1

deltaTime = time.process_time() - startTime

print("Total steps: " + str(totalStepsRun))
print("Total time: " + str(deltaTime))
print("Average steps per second: " + str(totalStepsRun / deltaTime))