# textworldexpress-web-server.py
#
#   Uses pywebio to open a simple web-based interface for running ScienceWorld in the web browser.
#   After running, open a web browser and point to 'localhost:8080'.
#   It may take a 5-10 seconds to initialize the server on the first run.
#
#   pip install -U pywebio (for web server)

from email.policy import default
import json
import time
import argparse
from datetime import datetime

import pywebio
import pywebio.output as pywebio_out

from textworld_express import TextWorldExpressEnv


class OutputLog:
    #
    # Constructor
    #
    def __init__(self):
        self.out = ""
        self.title = ""

    def setTitle(self, titleStr:str):
        self.title = titleStr

    def addHeading(self, strIn:str):
        self.out += "<h1>" + strIn + "</h1>\n"

    def addSubheading(self, strIn:str):
        self.out += "<h2>" + strIn + "</h2>\n"

    def addHorizontalRule(self):
        self.out += "<hr>\n"

    def addPreformattedText(self, strIn:str):
        self.out += "<pre>\n" + strIn + "\n</pre>\n"

    def addStr(self, strIn:str):
        self.out += strIn + "\n"

    def getHTML(self):
        out = "<html>"
        out += "<head><title>" + self.title + "</title></head>\n"
        out += "<body>\n"
        out += self.out
        out += "</body>\n"
        out += "</html>\n"

        return out

# #
# #   Save JSON history
# #
# def saveJSONHistory(history:list):
#     pathOut = "recordings/"
#     taskName = history[-1]['taskName']
#     varIdx = history[-1]['variationIdx']
#     score = history[-1]['score']

#     result = "success"
#     if (score != 1.0):
#         result = "failure"

#     # timestamp
#     dateTimeObj = datetime.now()
#     timestampStr = dateTimeObj.strftime("timestamp%Y-%b-%d-%H-%M-%S")

#     filenameOut = pathOut + "recording-" + str(taskName) + "-var" + str(varIdx) + "-" + str(result) + str(timestampStr) + ".json"

#     print ("Exporting " + filenameOut)

#     with open(filenameOut, "w") as jsonFile:
#         json.dump(history, jsonFile, indent=4, sort_keys=True)


#
#   Web server main
#
def app():
    exitCommands = ["quit", "exit"]

    simplificationStr = ""
    htmlLog = OutputLog()

    pywebio.session.set_env(title='TextWorldExpress Demo', auto_scroll_bottom=True)

    # Initialize environment
    env = TextWorldExpressEnv(serverPath=None, envStepLimit=100, threadNum=5)

    pywebio_out.put_markdown('## TextWorldExpress (Text Simulation)')
    #put_button("Click here to export transcript", onclick=lambda: , color='success', outline=True)

    htmlLog.addHeading("TextWorldExpress (Text Simulation)")
    htmlLog.addHorizontalRule()

    gameName = pywebio.input.select("Select a benchmark:", env.getGameNames())
    gameSeed = pywebio.input.input("Choose a seed (default: random):", type=pywebio.input.NUMBER, value=0)
    gameSeed = int(gameSeed)

    # Load environment
    gameFold = "train"
    gameParams = ""
    generateGoldPath = False
    env.load(gameName, gameFold, gameSeed, gameParams, generateGoldPath)

    # Initialize the game with a seed.
    if gameSeed:
        obs = env.resetWithSeed(gameSeed, gameFold, generateGoldPath)
    else:
        obs = env.resetWithRandomSeed(gameFold, generateGoldPath)

    pywebio_out.put_table([
        ["Benchmark", gameName],
        ["Seed", str(gameSeed)]
    ])

    htmlLog.addStr("<b>Benchmark:</b> " + gameName + "<br>")
    htmlLog.addStr("<b>Seed:</b> " + str(gameSeed) + "<br>")
    htmlLog.addHorizontalRule()

    historyRecording = []

    userInputStr = "look around"        # First action
    consoleMoveCount = 0
    while (userInputStr not in exitCommands):
        #put_markdown("### Move " + str(env.getNumMoves()) )
        #htmlLog.addSubheading("Move " + str(env.getNumMoves()))
        pywebio_out.put_markdown("### Move " + str(consoleMoveCount) )
        htmlLog.addSubheading("Move " + str(consoleMoveCount))

        # Send user input, get response
        obs = env.step(userInputStr)
        observation = obs['observation']
        score = obs['score']
        reward = obs['reward']
        isCompleted = obs['tasksuccess'] or obs['taskfailure']
        validActions = sorted(obs['validActions'])

        # Output (server)
        pywebio_out.put_text(observation)
        pywebio_out.put_table([
            ["Reward:", str(reward)],
            ["Score:", str(score)],
            ["isCompleted:", str(isCompleted)]
        ])

        # Output (log)
        htmlLog.addPreformattedText(observation)
        if (score >= 1.0):
            htmlLog.addStr("<font color=green>Task Score: " + str(score) + "  (isCompleted: " + str(isCompleted) + ") </font><br><br>")
        else:
            htmlLog.addStr("<font color=grey>Task Score: " + str(score) + "  (isCompleted: " + str(isCompleted) + ") </font><br><br>")

        logFilename = "log-" + gameName + ".html"
        pywebio_out.put_file(logFilename, htmlLog.getHTML().encode(), '(click here to export transcript)')

        #print("\n" + observation)
        #print("Score: " + str(score))
        #print("isCompleted: " + str(isCompleted))

        # Get user input
        userInputStr = pywebio.input.select('Select an action:', options=validActions)

        # Sanitize input
        # userInputStr = userInputStr.lower().strip()

        pywebio_out.put_text("> " + userInputStr)
        htmlLog.addStr("Selected Action:<br>")
        htmlLog.addStr("<i> > " + userInputStr + "</i><br>")

        # # Record history
        # packed = {
        #     'observation': observation,
        #     'reward': reward,
        #     'score': score,
        #     'isCompleted': isCompleted,
        #     'userInput': userInputStr,
        #     'gameName': gameName,
        #     'taskDescription': env.getTaskDescription(),
        #     'look': env.look(),
        #     'inventory': env.inventory(),
        #     # 'variationIdx': variationIdx,
        #     'consoleMoveCount': consoleMoveCount,
        # }
        # historyRecording.append(packed)

        # # If this session is completed, save the recording
        # if (isCompleted == True):
        #     saveJSONHistory(historyRecording)

        consoleMoveCount += 1

        time.sleep(1)


    print("Shutting down server...")
    env.shutdown()

    print("Completed.")


def parse_args():
    desc = "Launch a webserver to interact with ScienceWorld from your browser."
    parser = argparse.ArgumentParser(desc)
    parser.add_argument("--port", type=int, default=8080,
                        help="Port to use for the webserver.")
    parser.add_argument("--debug", action="store_true",
                        help="Run webserver in debug mode.")

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_args()
    pywebio.start_server(app, port=args.port, debug=args.debug)
