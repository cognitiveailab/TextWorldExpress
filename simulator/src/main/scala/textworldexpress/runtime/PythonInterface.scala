package textworldexpress.runtime

import py4j.GatewayServer
import textworldexpress.generator.GameGenerator
import textworldexpress.struct.{StepResult, TextGame}

import collection.JavaConverters._


class PythonInterface() {
  val ERROR_MESSAGE_UNINITIALIZED = "ERROR: Interface is not initialized -- call reset() before beginning."
  var errorStr:String = ""

  // Game and gold path
  var gameGenerator:GameGenerator = null
  var game:TextGame = null
  var goldPath:Array[String] = Array.empty[String]
  var properties:Map[String, Int] = Map[String, Int]()
  var curStepResult:StepResult = null

  /*
   * Load/reset/shutdown server
   */
  def load(gameName:String, gameFold:String, seed:Int, paramStr:String, generateGoldPath:Boolean):StepResult = {
    // Clear variables
    this.game = null
    this.goldPath = Array.empty[String]
    this.errorStr = ""
    this.curStepResult = null

    // Step 1: Convert properties from string
    // TODO: Convert paramStr to properties
    println ("TODO: PARSE PROPERTIES STRING")

    // Step 2: Create the Game Generator
    val (success, gameGenerator) = GameGenerator.mkGameGenerator(gameName, properties)
    if (!success) {
      println ("ERROR creating Game Generator: ")
      println (gameGenerator.errorStr)
      errorStr = gameGenerator.errorStr
      return StepResult.mkErrorMessage(errorStr)
    }

    // Step 3: Generate new game
    return this.generateNewGame(seed, gameFold, generateGoldPath)
  }

  // Mirror with JSON output
  def loadJSON(gameName:String, gameFold:String, seed:Int, paramStr:String, generateGoldPath:Boolean):String = {
    val stepResult = this.load(gameName, gameFold, seed, paramStr, generateGoldPath)
    stepResult.toJSON()
  }


  // Assumes that load() has already been called, and gameGenerator is valud.
  def generateNewGame(seed:Int, gameFold:String, generateGoldPath:Boolean):StepResult = {
    if (gameGenerator == null) {
      errorStr = "ERROR: Game generator is not initialized.  Call load() before attempting to generate new games."
      return StepResult.mkErrorMessage(errorStr)
    }

    // Generate new game
    if (generateGoldPath) {
      // With gold path
      val (_game, _goldPath) = gameGenerator.mkGameWithGoldPath(seed, gameFold)
      game = _game
      goldPath = _goldPath

      // Check gold path is present
      if (goldPath.length == 0) this.errorStr = "ERROR: Unable to generate gold path."
    } else {
      // Without gold path
      game = gameGenerator.mkGame(seed, gameFold)
    }

    // Take first 'step'
    this.curStepResult = game.initalStep()
    return this.curStepResult
  }

  // Mirror with JSON output
  def generateNewGameJSON(seed:Int, gameFold:String, generateGoldPath:Boolean):String = {
    val stepResult = this.generateNewGame(seed, gameFold, generateGoldPath)
    stepResult.toJSON()
  }


  def resetWithRandomSeed(gameFold:String, generateGoldPath:Boolean):StepResult = {
    // Step 1: Create random seed according to fold
    var randSeed = -1
    gameFold match {
      case "train" => randSeed = this.getRandomSeedTrain()
      case "dev" => randSeed = this.getRandomSeedDev()
      case "test" => randSeed = this.getRandomSeedTest()
      case _ => {
        this.errorStr = "ERROR: Unknown game fold (" + gameFold + ").  Valid options are (train, dev, test)."
        return StepResult.mkErrorMessage(this.errorStr)
      }
    }

    // Step 2: Generate new game
    return this.generateNewGame(seed = randSeed, gameFold, generateGoldPath)
  }

  // Mirror with JSON output
  def resetWithRandomSeedJSON(gameFold:String, generateGoldPath:Boolean):String = {
    val stepResult = this.resetWithRandomSeed(gameFold, generateGoldPath)
    stepResult.toJSON()
  }


  // Shutdown server
  def shutdown(): Unit = {
    sys.exit(0)
  }


  /*
   * Get valid tasks/environments
   */
  def getTaskNames():java.util.List[String] = {
    GameGenerator.VALID_GAME_NAMES.toList.asJava
  }


  /*
   * Train/development/test sets
   */

  def getSeedsTrain():java.util.List[Int] = {
    return Range(0, 1000).toList.asJava
  }

  def getSeedsDev():java.util.List[Int] = {
    return Range(10000, 11000).toList.asJava
  }

  def getSeedsTest():java.util.List[Int] = {
    return Range(20000, 21000).toList.asJava
  }


  def getRandomSeedTrain():Int = {
    val randSeed = scala.util.Random.nextInt(1000) + 0
    return randSeed
  }

  def getRandomSeedDev():Int = {
    val randSeed = scala.util.Random.nextInt(1000) + 10000
    return randSeed
  }

  def getRandomSeedTest():Int = {
    val randSeed = scala.util.Random.nextInt(1000) + 20000
    return randSeed
  }


  /*
   * Gold action sequence
   */
  def getGoldActionSequence():java.util.List[String] = {
    return this.goldPath.toList.asJava
  }

  /*
   * History
   */

  /*
  def getActionHistory():java.util.List[String] = {
    return this.currentHistory.historyActions.toList.asJava
  }

  // Get entire run history for this instance in JSON format
  def getRunHistoryJSON():String = {
    return this.currentHistory.toJSON()
  }
   */


  /*
   * Take action steps and get observations/scores
   */

  def getCompleted():Boolean = {
    if (this.curStepResult == null) return true
    if (this.curStepResult.taskSuccess || this.curStepResult.taskFailure) return true

    // Otherwise
    return false
  }

  // Normal
  def step(userInputString:String):StepResult = {
    // TODO: Add 'reward' in addition to 'score'?
    println ("TODO: Add reward in addition to score")

    // Error checking
    if (this.errorStr != "") return StepResult.mkErrorMessage(this.errorStr)
    if (this.game == null) return StepResult.mkErrorMessage(this.ERROR_MESSAGE_UNINITIALIZED)

    // Sanitize user input
    val userInputSanitized = userInputString.trim

    // Check for valid action
    if (!this.curStepResult.validActions.contains(userInputSanitized)) {
      return StepResult.mkInvalidStep(this.curStepResult)
    }

    // Take step
    this.curStepResult = game.step(userInputSanitized)

    // Return
    return this.curStepResult
  }

  // Mirror with JSON output
  def stepJSON(userInputString:String):String = {
    val stepResult = this.step(userInputString)
    stepResult.toJSON()
  }

}

object PythonInterface {

  def printUsage(): Unit = {
    println("Usage: PythonInterface <portNumber>")
  }

  def main(args:Array[String]): Unit = {
    println ("Initializing TextWorldExpress Python Server...")
    val obj = new PythonInterface()

    // Parse command line argument for port
    var port:Int = 25335      // Default port (if not specified)
    if (args.length == 1) {
      try {
        port = args(0).toInt
      } catch {
        case e:Throwable => {
          printUsage()
          throw new RuntimeException("ERROR: Unable to parse port number into integer(" + args(0) + ").")
        }
      }
    } else if (args.length > 1) {
      printUsage()
      sys.exit(1)
    }

    println ("Starting server on port " + port + ".")

    val server = new GatewayServer(obj, port)
    server.start()

    println ("Server started... ")

  }

}
