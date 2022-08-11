package textworldexpress.games

import textworldexpress.objects.{FastObject}
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}



// 'taskObjects' just contains the reference to any coin(s) to be collected in the agent's inventory
class SimonSaysGameScoring(val goldActionSequence:Array[String], lastValidActions:ListBuffer[(String, Int, Array[FastObject])]) extends Scorer {

  def doScoring(): Unit = {
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false

    // Check: Make sure the number of actions taken is less than or equal to the number of gold actions requested
    if (goldActionSequence.length > lastValidActions.length) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // Check each action taken by the agent, to make sure it was the correct (gold) action
    for (i <- 0 until lastValidActions.length) {
      val actionStr = lastValidActions(i)._1

      if (goldActionSequence(i) == actionStr) {
        // Chose the right option here
        curScore += 1
      } else {
        // Task failure
        taskFailure = true
      }

    }

    // If one action was incorrect, then task failure
    if (taskFailure) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // If all actions have been taken, and all were correct, then task success
    if (goldActionSequence.length == lastValidActions.length) {
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



class SimonSaysGame(val goldActionSequence:Array[String], val possibleActions:Array[String], val seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // Scorer
  val scorer:Scorer = new SimonSaysGameScoring(goldActionSequence, lastValidActions)

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Internal game random number generator -- primarily for randomizing valid action list.
  val random = new Random(seed)

  /*
   * Cloning
   */

  // TODO: Not implemented
  def deepCopy():SimonSaysGame = {
    println ("NOTE: deepCopy() not implemented -- returning a shallow copy")
    // Return
    return new SimonSaysGame(goldActionSequence, possibleActions, seed, generationProperties)
  }



  /*
   * Generation Properties
   */

  def getGenerationProperties():Map[String, Int] = return this.generationProperties


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
  def mkObservation():String = {
    val curStage = this.lastValidActions.length

    // Check for task completion
    if (curStage > this.goldActionSequence.length) {
      return "Task Completed."
    }

    val os = "Simon says, take this action: " + this.goldActionSequence(curStage)
    return os
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

    // Do scoring
    scorer.doScoring()

    // Get current score
    val curScores = this.getScore()

    // Generate next valid actions
    val validActions = this.mkActions(ListBuffer.empty[FastObject])
    lastValidActions = validActions
    var validActionStrs = new ArrayBuffer[String](validActions.length)
    for (i <- 0 until validActions.length) validActionStrs.append(validActions(i)._1)
    validActionStrs = random.shuffle(validActionStrs)


    // Generate observation, free-look, and inventory strings
    val observationStr = this.mkObservation()                       //## Special to this task: Observation is generated, rather than coming from environment
    val freeLookStr = ""
    val inventoryStr = ""

    // Add to action history
    if (actionNumber >= 0) {
      this.history.append(new ActionHistory(actionStr, observationStr, curScores))
    }

    // Return
    val result = new StepResult(observationStr=observationStr, freeLookStr=freeLookStr, inventoryStr=inventoryStr, validActions = validActionStrs.toArray, scoreRaw=curScores.scoreRaw, scoreNormalized=curScores.scoreNormalized, taskSuccess=curScores.taskSuccess, taskFailure=curScores.taskFailure, wasValidAction = wasValidAction)
    return result
  }


}


class SimonSaysGameGenerator {

  // Make a random sequence of actions
  def mkActionSequence(r:Random, gameFold:String, length:Int):Array[String] = {
    // Step 1: Object actions
    val objectNamesTrain  = Array(("apple", "apples"), ("orange", "oranges"), ("grape", "grapes"), ("tangerine", "tangerines"), ("banana", "bananas"), ("pineapple", "pineapples"), ("papaya", "papayas"), ("peach", "peaches"), ("strawberry", "strawberries"), ("grapefruit", "grapefruits") )
    val objectNamesDev    = Array(("broccoli", "brocollis"), ("onion", "onions"), ("cucumber", "cucumbers"), ("potato", "potatoes"), ("cucumber", "cucumbers"), ("coconut", "coconuts"), ("watermelon", "watermelons"), ("mango", "mangos"), ("olive", "olives"), ("lime", "limes"), ("pear", "pears") )
    val objectNamesTest   = Array(("pepper", "peppers"), ("tomato", "tomatoes"), ("eggplant", "eggplants"), ("squash", "squashes"), ("pumpkin", "pumpkins"), ("pea", "peas"), ("avocado", "avocados"), ("cabbage", "cabbages"), ("prune", "prunes"), ("blueberry", "blueberries") )

    val numObjects:Int = 4

    // Find appropriate set, and shuffle order
    val objectNames:Array[(String, String)] = if (gameFold == "train") { objectNamesTrain } else if (gameFold == "dev") { objectNamesDev } else if (gameFold == "test") { objectNamesTest } else { Array.empty[(String, String)] }
    val shuffled = r.shuffle(r.shuffle(objectNames.toList)).toArray   // Double shuffle, so the order is different than other games

    // Create task objects, in order
    val actionsOut = new ArrayBuffer[String]
    for (i <- 0 until numObjects) {
      val objName = shuffled(i)._1
      actionsOut.append("take the " + objName)
      actionsOut.append("eat the " + objName)
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
    val out = shuffledOut.slice(0, length)

    // Return
    return shuffledOut.toArray
  }


  def mkGame(seed:Long, fold:String = "train"):SimonSaysGame = {
    val r = new Random(seed)
    val gameLength:Int = r.nextInt(2) + 3   // Game lengths between 3-5 actions

    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1
    props("gameLength") = gameLength

    // Generate Game
    val actionSequence = this.mkActionSequence(r, gameFold = fold, length = gameLength+2)   // Overgenerate by 2, to have distractors
    val goldSequence = actionSequence.slice(0, gameLength)
    val possibleActions = actionSequence

    val game = new SimonSaysGame(goldSequence, possibleActions, seed, generationProperties = props.toMap )

    return game
  }


  def mkGameWithGoldPath(seed:Long, fold:String = "train"):(SimonSaysGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 50
    val rg = new Random()

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, fold)
        val goldAgent = new SimonSaysGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, fold)
    return (game, goldPath)
  }


}



