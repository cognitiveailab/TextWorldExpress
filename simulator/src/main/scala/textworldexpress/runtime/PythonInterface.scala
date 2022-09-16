package textworldexpress.runtime

import py4j.GatewayServer
import textworldexpress.generator.GameGenerator
import textworldexpress.struct.{StepResult, TextGame}
import textworldexpress.symbolicmodule.SymbolicModuleInterface

import collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class PythonInterface() {
  val ERROR_MESSAGE_UNINITIALIZED = "ERROR: Interface is not initialized -- call reset() before beginning."
  var errorStr:String = ""

  // Game and gold path
  var gameGenerator:GameGenerator = null
  var game:TextGame = null
  var gameName:String = ""
  var gameFold:String = ""
  var paramStr:String = ""
  var enabledModulesStr:String = ""
  var gameSeed:Int = 0
  var goldPath:Array[String] = Array.empty[String]
  var properties:Map[String, Int] = Map[String, Int]()
  var curStepResult:StepResult = null

  var enabledModuleStrs:Array[String] = Array.empty[String]
  var moduleInterface:SymbolicModuleInterface = null

  var history = new ArrayBuffer[StepHistory]

  // TODO: This is not a full deep copy, but a mostly-deep copy for all the places that would require fast path crawling for a single game.
  def deepCopy():PythonInterface = {
    val clonedInterface = new PythonInterface()
    clonedInterface.gameGenerator = this.gameGenerator

    clonedInterface.game = this.game.deepCopy()

    clonedInterface.gameName = this.gameName
    clonedInterface.gameFold = this.gameFold
    clonedInterface.paramStr = this.paramStr
    clonedInterface.enabledModulesStr = this.enabledModulesStr
    clonedInterface.gameSeed = this.gameSeed
    clonedInterface.goldPath = this.goldPath
    clonedInterface.properties = this.properties
    clonedInterface.curStepResult = this.curStepResult

    clonedInterface.enabledModuleStrs = this.enabledModuleStrs
    clonedInterface.moduleInterface = this.moduleInterface
    for (i <- 0 until this.history.length) {
      clonedInterface.history.append( this.history(i) )
    }

    return clonedInterface
  }

  /*
   * Load/reset/shutdown server
   */
  def load(gameName:String, gameFold:String, seed:Int, paramStr:String, generateGoldPath:Boolean, enabledModulesStr:String = ""):StepResult = {
    // Clear variables
    this.game = null
    this.gameName = gameName
    this.gameFold = gameFold
    this.gameSeed = seed
    this.paramStr = paramStr
    this.enabledModulesStr = enabledModulesStr
    this.goldPath = Array.empty[String]
    this.errorStr = ""
    this.curStepResult = null
    this.gameGenerator = null
    this.moduleInterface = null
    this.history = new ArrayBuffer[StepHistory]

    // Step 1: Parse any properties passed in through the string
    val (_props, propErrorStr) = PythonInterface.parseParamStr(paramStr)
    if (propErrorStr.length > 0) return StepResult.mkErrorMessage(errorStr)
    this.properties = _props

    // Step 2: Create the Game Generator
    val (success, gameGenerator) = GameGenerator.mkGameGenerator(gameName, this.properties)
    if (!success) {
      println ("ERROR creating Game Generator: ")
      println (gameGenerator.errorStr)
      errorStr = gameGenerator.errorStr
      return StepResult.mkErrorMessage(errorStr)
    }
    this.gameGenerator = gameGenerator

    // Step 3: Generate new game
    this.curStepResult = this.generateNewGame(seed, gameFold, generateGoldPath)

    // Step 4: Initialize any symbolic modules
    // Parse the module request string
    this.enabledModuleStrs = enabledModulesStr.split(",").map(_.trim)
    // Add symbolic modules
    this.resetSymbolicModules()

    // Add actions from symbolic modules
    this.addValidActionsFromModules()

    // Add to history
    this.history.append( new StepHistory(actionStr = "", moduleStr = "", this.curStepResult) )

    return this.curStepResult
  }

  // Mirror with JSON output
  def loadJSON(gameName:String, gameFold:String, seed:Int, paramStr:String, generateGoldPath:Boolean, enabledModulesStr:String = ""):String = {
    this.curStepResult = this.load(gameName, gameFold, seed, paramStr, generateGoldPath)
    this.addValidActionsFromModules()
    return this.curStepResult.toJSON()
  }

  // Reset the symbolic modules
  def resetSymbolicModules(): Unit = {
    moduleInterface = null
    // Reset
    moduleInterface = new SymbolicModuleInterface(properties = game.getGenerationProperties())
    // Add requested modules
    for (enabledModuleStr <- this.enabledModuleStrs) {
      moduleInterface.addModule(moduleName = enabledModuleStr)
    }
    // TODO: Check for errors (e.g. modules that do not exist)
  }

  // Adds any additional valid actions from the symbolic modules into the action space
  def addValidActionsFromModules(): Unit = {
    if (this.moduleInterface != null) {
      // Update modules with environment status
      moduleInterface.giveEnvironmentStatus(this.curStepResult.observationStr, this.curStepResult.inventoryStr, this.curStepResult.freeLookStr)

      // Get valid actions from modules
      val newValidActions = (this.curStepResult.validActions ++ this.moduleInterface.getValidCommands()).toSet.toArray    // Remove any duplicates
      //println ("### ACTIONS FROM MODULE: " + this.moduleInterface.getValidCommands().mkString(", "))
      //println ("### ADDING VALID ACTIONS: " + newValidActions.mkString(", "))
      this.curStepResult = this.curStepResult.cloneButReplaceValidActions(newValidActions = newValidActions)
    }
  }

  // Assumes that load() has already been called, and gameGenerator is valid.
  def generateNewGame(seed:Int, gameFold:String, generateGoldPath:Boolean):StepResult = {
    if (this.gameGenerator == null) {
      errorStr = "ERROR: Game generator is not initialized.  Call load() before attempting to generate new games."
      return StepResult.mkErrorMessage(errorStr)
    }

    // Store/reset generation parameters
    this.gameSeed = seed
    this.gameFold = gameFold
    this.history = new ArrayBuffer[StepHistory]

    // Generate new game
    if (generateGoldPath) {
      // With gold path

      // Case 1: Without symbolic modules
      if ((this.moduleInterface == null) || (this.moduleInterface.getEnabledModuleNames().length == 0)) {
        val (_game, _goldPath) = this.gameGenerator.mkGameWithGoldPath(seed, gameFold)
        this.game = _game
        this.goldPath = _goldPath
      } else {
        // Case 2: With symbolic modules (requires going through Interface instead of Game
        // First, create a faux interface that duplicates this one
        val fauxInterface = new PythonInterface()
        fauxInterface.load(gameName = this.gameName, gameFold = this.gameFold, seed = this.gameSeed, paramStr = this.paramStr, generateGoldPath = false, enabledModulesStr = this.enabledModulesStr)
        // Then, call the gold agent with this interface
        val rg = new Random(seed)
        println ("### HERE")
        val (success, _goldPath) = this.gameGenerator.mkGoldPathModules(rg, fauxInterface)
        this.goldPath = _goldPath
        if (!success) println ("### ERROR: Unable to generate gold path with module interface.")
        // Also create the game
        this.game = this.gameGenerator.mkGame(seed, gameFold)

      }

      // Check gold path is present
      if (goldPath.length == 0) this.errorStr = "ERROR: Unable to generate gold path."
    } else {
      // Without gold path
      this.game = this.gameGenerator.mkGame(seed, gameFold)
    }

    // Reset the symbolic modules
    this.resetSymbolicModules()

    // Take first 'step'
    this.curStepResult = game.initalStep()
    addValidActionsFromModules()

    // Add to history
    this.history.append( new StepHistory(actionStr = "", moduleStr = "", this.curStepResult) )

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
    this.curStepResult = this.generateNewGame(seed = randSeed, gameFold, generateGoldPath)
    this.addValidActionsFromModules()
    return this.curStepResult
  }

  // Mirror with JSON output
  def resetWithRandomSeedJSON(gameFold:String, generateGoldPath:Boolean):String = {
    this.curStepResult = this.resetWithRandomSeed(gameFold, generateGoldPath)
    this.addValidActionsFromModules()
    this.curStepResult.toJSON()
  }


  // Shutdown server
  def shutdown(): Unit = {
    sys.exit(0)
  }


  /*
   * Get valid tasks/environments
   */
  def getGameNames():java.util.List[String] = {
    GameGenerator.VALID_GAME_NAMES.toList.asJava
  }

  /*
   * Get generation properties
   */
  def getGenerationPropertiesJSON():String = {
    if (this.game == null) return "{}"

    val props = this.game.getGenerationProperties()

    val propStrs = new ArrayBuffer[String]
    for (propName <- props.keySet) {
      propStrs.append("\"" + propName + "\": " + props(propName))
    }

    return "{" + propStrs.mkString(", ") + "}"
  }

  /*
   * Get task description
   */
  def getTaskDescription():String = {
    if (this.game == null) return "Task description unavailable -- game is not initialized."

    return this.game.getTaskDescription()
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
    // Error checking
    if (this.errorStr != "") return StepResult.mkErrorMessage(this.errorStr)
    if (this.game == null) return StepResult.mkErrorMessage(this.ERROR_MESSAGE_UNINITIALIZED)

    // Sanitize user input
    val userInputSanitized = userInputString.trim

    // Valid actions (with valid actions from the module, if enabled)
    val validActions =
      if (this.moduleInterface == null) { this.curStepResult.validActions }
      else { this.curStepResult.validActions ++ this.moduleInterface.getValidCommands() }

    // Check for valid action
    if (!validActions.contains(userInputSanitized)) {
      return StepResult.mkInvalidStep(this.curStepResult)
    }

    // Take step
    var wasModuleCommand:Boolean = false
    // First, check to see if the action is intended for the symbolic module
    if (this.moduleInterface != null) {
      val (success, resultStr) = this.moduleInterface.runCommand(userInputSanitized)
      if (success) {
        // The command was successfully run by a symbolic module.
        // Reuse the last curStepResult (since a module command shouldn't change the environment), but just replace the old observation with the output of the module
        wasModuleCommand = true
        this.curStepResult = this.curStepResult.cloneButReplaceObservation(newObservationStr = resultStr)

      } else {
        // The command was not run by the module -- run in the environment
        this.curStepResult = game.step(userInputSanitized)
      }
    } else {
      // Modules not enabled -- run in the environment
      this.curStepResult = game.step(userInputSanitized)
    }

    // Add any actions available from the symbolic modules to the action space
    this.addValidActionsFromModules()

    // Add to history
    if (wasModuleCommand) {
      this.history.append(new StepHistory(actionStr = "", moduleStr = userInputSanitized, this.curStepResult))
    } else {
      this.history.append(new StepHistory(actionStr = userInputSanitized, moduleStr = "", this.curStepResult))
    }

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

  /*
   * Helper functions
   */
  // Parse a string of comma-delimited parameters into a Map.
  // Returns (output, errorStr).
  def parseParamStr(strIn:String):(Map[String, Int], String) = {
    val out = mutable.Map[String, Int]()

    // Step 1: Split on deliminter (",")
    val params = strIn.split(",").map(_.trim())
    for (paramStr <- params) {
      if (paramStr.length > 0) {
        val fields = paramStr.split("=").map(_.trim)
        if (fields.length == 0) return (out.toMap, "ERROR: Unable to parse field.")
        if (fields.length == 1) return (out.toMap, "ERROR: Unable to find value for parameter (" + fields(0) + ").")
        if (fields.length > 2)  return (out.toMap, "ERROR: Too many fields found (" + fields.mkString(" ") + ").  A comma is likely missing between fields, or there is an extra equals sign.")
        val paramName = fields(0)
        val paramValue = fields(1)
        var paramValueInt:Int = -1
        try {
          paramValueInt = paramValue.toInt
        } catch {
          case _:Throwable => { return (out.toMap, "ERROR: Unable to parse parameter (" + paramName + ") value (" + paramValue + ") into an integer.")}
        }

        // Store
        out(paramName) = paramValueInt
      }
    }

    return (out.toMap, "")
  }

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
