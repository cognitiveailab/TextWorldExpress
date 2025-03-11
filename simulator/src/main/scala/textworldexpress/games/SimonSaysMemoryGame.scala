package textworldexpress.games

import textworldexpress.goldagent.SimonSaysMemoryGoldAgent
import textworldexpress.objects.FastObject
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

// GoldActionSequences contains the list of action sequences that the agent should repeat.
class SimonSaysMemoryGameScoring(val goldActionSequence:Array[String], history:ArrayBuffer[ActionHistory]) extends Scorer {

  def doScoring(): Unit = {
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false

    // Check: Make sure the number of actions taken is less than or equal to the number of gold actions requested
    if (history.length-1 > goldActionSequence.length) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // Check each action taken by the agent, to make sure it was the correct (gold) action
    if (history.length > 1) {
      for (i <- 1 until history.length) {
        val actionStr = history(i).actionStr
        //println("history(" + i + "): " + actionStr + "    " + history(i).scores.scoreNormalized)

        if (goldActionSequence(i-1) == actionStr) {
          // Chose the right option here
          curScore += 1
        } else {
          // Task failure
          taskFailure = true
        }

      }
    }

    // If one action was incorrect, then task failure
    if (taskFailure) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // If all actions have been taken, and all were correct, then task success
    if (goldActionSequence.length == history.length-1) {
      taskSuccess = true
    }


    // Store current scores in 'this.curScore', for the accessor in the base class
    val normalizedScore = curScore/maxScore
    val scores = new GameScore(scoreRaw = curScore, scoreNormalized = normalizedScore, taskSuccess = taskSuccess, taskFailure = taskFailure)
    this.curScore = scores
  }

  // Calculate the maximum possible score for this task (the number of actions in the gold sequence)
  def calculateMaxScore():Double = {
    var maxScore:Double = this.goldActionSequence.size.toDouble
    return maxScore
  }

}



class SimonSaysMemoryGame(val goldActionSequence:Array[String], val possibleActions:Array[String], val seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Scorer
  val scorer:Scorer = new SimonSaysGameScoring(goldActionSequence, history)

  // Internal game random number generator -- primarily for randomizing valid action list.
  val random = new Random(seed)

  // Current step
  var currentStep:Int = 0

  /*
   * Cloning
   */

  // TODO: Not implemented
  def deepCopy():SimonSaysMemoryGame = {
    println ("NOTE: deepCopy() not implemented -- returning a shallow copy")
    // Return
    return new SimonSaysMemoryGame(goldActionSequence, possibleActions, seed, generationProperties)
  }



  /*
   * Generation Properties
   */

  def getGenerationProperties():Map[String, Int] = return this.generationProperties

  /*
   * Task Description
   */
  def getTaskDescription():String = {
    return "Your task is to do exactly what Simon says."
  }

  /*
   * History
   */
  def getHistory():ArrayBuffer[ActionHistory] = {
    return this.history
  }

  /*
   * Score
   */
  // Returns (score, taskSuccess, taskFailure)
  def getScore():GameScore = this.scorer.getCurrentScore()

  /*
   * Actions
   */

  def actionSimonSays(params:Array[FastObject]):String = this.actionSimonSays()
  def actionSimonSays():String = {
    return ""
  }

  val ACTION_SIMONSAYS    = 18

  val ACTION_INVALID      = 0


  /*
   * Action runner/interpreter
   */
  def runAction(actionIdx:Int, params:Array[FastObject]):String = {

    actionIdx match {
      case ACTION_SIMONSAYS => return this.actionSimonSays(params)

      case _ => return "That is not a command that I recognize."
    }

  }

  /*
   * Action generation
   */
  def mkActions(visibleObjects:ListBuffer[FastObject]): ListBuffer[ (String, Int, Array[FastObject]) ] = {
    val actionsOut = new ListBuffer[(String, Int, Array[FastObject])]    // (action, callback function, parameters)

    // Generic simon says action
    for (i <- 0 until this.possibleActions.length) {
      val possibleActionStr = this.possibleActions(i)
      actionsOut.append( (possibleActionStr, ACTION_SIMONSAYS, Array.empty[FastObject]) )
    }

    return actionsOut
  }

  /*
   * Make current observation for Simon Says
   */
  def mkObservation(curAction: String, curStage:Int):String = {

    if (curStage == 0) {
      // First step
      return "Simon says, take these actions in order: " + this.goldActionSequence.mkString(", ") + "."
    }

    if (curStage >= this.goldActionSequence.length) {
      // Last step
      return "Simon says, you have completed the game.  You win!"
    }

    if (curAction != this.goldActionSequence(curStage-1)) {
      return "Incorrect!"
    }

    if (this.generationProperties("verbose") == 1) {
      return "Correct!"
    }

    return ""
  }


  /*
   * Step
   */

  def initalStep():StepResult = {
    // By giving '-1' as an actionNumber, 'step()' will generate the observation without running the action or including it in the history.
    return this.step("", -1, Array.empty[FastObject])
  }

  def step(actionStr:String):StepResult = {
    for (action <- lastValidActions) {
      if (action._1 == actionStr) {
        return this.step(actionStr, action._2, action._3)
      }
    }
    // If we reach here, the action was invalid
    return this.step(actionStr, ACTION_INVALID, Array.empty[FastObject])
  }

  def step(validActionIdx:Int):StepResult = {
    val action = this.lastValidActions(validActionIdx)
    return this.step(action._1, action._2, action._3)
  }

  def step(actionStr:String, actionNumber:Int, actionParams:Array[FastObject]):StepResult = {

    // Run action
    if (actionNumber >= 0) {
      // Only run the action if actionNumber is positive -- this is used as a way of generating the initial observation by calling this method with actionNumber = -1.
      val unused = this.runAction(actionNumber, actionParams)
    }
    val wasValidAction = if (actionNumber == ACTION_INVALID) { false } else { true }    // If the action is valid, true, otherwise, false


    // Generate next valid actions
    val validActions = this.mkActions(ListBuffer.empty[FastObject])
    lastValidActions = validActions
    var validActionStrs = new ArrayBuffer[String](validActions.length)
    for (i <- 0 until validActions.length) validActionStrs.append(validActions(i)._1)
    validActionStrs = random.shuffle(validActionStrs)


    // Generate observation, free-look, and inventory strings
    val freeLookStr = ""
    val inventoryStr = ""

    val observationStr = this.mkObservation(curAction = actionStr, curStage = this.currentStep)                       //## Special to this task: Observation is generated, rather than coming from environment

    // This part is a bit weird -- the scorer uses the history to generate the score.  But the history stores the score, so there's a circular reference.
    // So, we add a faux entry to the history -- calculate the score, then pop it off and put the real one on.
    // Add to action history
    this.history.append(new ActionHistory(actionStr, observationStr, this.getScore()))

    // Do scoring
    scorer.doScoring()

    // Get current score
    val curScores = this.getScore()

    // The strange part
    this.history.remove(this.history.size-1)
    this.history.append(new ActionHistory(actionStr, observationStr, curScores))

    this.currentStep += 1

    // Return
    val result = new StepResult(observationStr=observationStr, freeLookStr=freeLookStr, inventoryStr=inventoryStr, validActions = validActionStrs.toArray, scoreRaw=curScores.scoreRaw, scoreNormalized=curScores.scoreNormalized, taskSuccess=curScores.taskSuccess, taskFailure=curScores.taskFailure, wasValidAction = wasValidAction)
    return result
  }


}


class SimonSaysMemoryGameGenerator {

  // Make a random sequence of actions
  def mkActionSequence(r:Random, gameFold:String, length:Int):Array[String] = {
    // Step 1: Object actions
    val objectNamesTrain  = Array(("apple", "apples"), ("orange", "oranges"), ("grape", "grapes"), ("tangerine", "tangerines"), ("banana", "bananas"), ("pineapple", "pineapples"), ("papaya", "papayas"), ("peach", "peaches"), ("strawberry", "strawberries"), ("grapefruit", "grapefruits") )
    val objectNamesDev    = Array(("broccoli", "brocollis"), ("onion", "onions"), ("cucumber", "cucumbers"), ("potato", "potatoes"), ("cucumber", "cucumbers"), ("coconut", "coconuts"), ("watermelon", "watermelons"), ("mango", "mangos"), ("olive", "olives"), ("lime", "limes"), ("pear", "pears") )
    val objectNamesTest   = Array(("pepper", "peppers"), ("tomato", "tomatoes"), ("eggplant", "eggplants"), ("squash", "squashes"), ("pumpkin", "pumpkins"), ("pea", "peas"), ("avocado", "avocados"), ("cabbage", "cabbages"), ("prune", "prunes"), ("blueberry", "blueberries") )

    val numObjects:Int = 10

    // Find appropriate set, and shuffle order
    val objectNames:Array[(String, String)] = if (gameFold == "train") { objectNamesTrain } else if (gameFold == "dev") { objectNamesDev } else if (gameFold == "test") { objectNamesTest } else { Array.empty[(String, String)] }
    val shuffled = r.shuffle(r.shuffle(objectNames.toList)).toArray   // Double shuffle, so the order is different than other games

    // Create task objects, in order
    val actionsOut = new ArrayBuffer[String]
    for (i <- 0 until numObjects) {
      val objName = shuffled(i)._1
      actionsOut.append("take " + objName)
      actionsOut.append("eat " + objName)
      actionsOut.append("examine " + objName)
      actionsOut.append("smell " + objName)
      actionsOut.append("touch " + objName)
      actionsOut.append("slice " + objName)
    }

    // Step 2: Action actions (from canonical simon says games)
    val actionsTrain = Array("sit down", "turn around in a circle", "jump up and down", "hop on your right foot", "meow like a cat")
    val actionsDev = Array("hop on your left foot", "clap your hands", "touch your nose", "wiggle your fingers", "quack like a duck")
    val actionsTest = Array("hop to the left", "touch your ears", "do a dance", "wave hello", "wiggle your toes")

    if (gameFold == "train") {
      actionsOut.insertAll(actionsOut.length, actionsTrain)
    } else if (gameFold == "dev") {
      actionsOut.insertAll(actionsOut.length, actionsDev)
    } else if (gameFold == "test") {
      actionsOut.insertAll(actionsOut.length, actionsTest)
    }

    // Shuffle final actions
    val shuffledOut = r.shuffle(actionsOut).toArray

    // Sample from the shuffled list, and add to the end
    val out = new ArrayBuffer[String]
    for (i <- 0 until length) {
      val idx = r.nextInt(shuffledOut.length)
      out.append(shuffledOut(idx))
    }

    // Return
    return out.toArray
  }


  def mkGame(seed:Long, gameLength:Int = 5, numDistractors:Int = 3, verbose:Int = 0, fold:String = "train"):SimonSaysMemoryGame = {
    val r = new Random(seed)

    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("gameLength") = gameLength
    props("numDistractors") = numDistractors
    props("verbose") = verbose
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1

    // Generate Game
    val actionSequence = this.mkActionSequence(r, gameFold = fold, length = gameLength+numDistractors)  // Overgenerate by numDistractors.
    val goldSequence = actionSequence.slice(0, gameLength)
    val possibleActions = actionSequence

    val game = new SimonSaysMemoryGame(goldSequence, possibleActions, seed, generationProperties = props.toMap )

    return game
  }


  def mkGameWithGoldPath(seed:Long, gameLength:Int = 5, numDistractors:Int = 3, verbose:Int = 0, fold:String = "train"):(SimonSaysMemoryGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 50
    val rg = new Random()

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, gameLength, numDistractors, verbose, fold)
        val goldAgent = new SimonSaysMemoryGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, gameLength, numDistractors, verbose, fold)
    return (game, goldPath)
  }


}



