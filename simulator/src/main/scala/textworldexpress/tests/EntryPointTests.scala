package textworldexpress.tests

import textworldexpress.generator.GameGenerator
import textworldexpress.struct.TextGame

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import scala.util.Random

/*
 * A quick test to check if the games and their gold agents are working correctly using default game parameters
 */
object EntryPointTests {

  def testGame(gameName:String, gameFold:String, numEnvs:Int = 10000, numStepsPerEnv:Int = 50, properties:Map[String, Int] = Map[String, Int]()): Int = {
    val goldPathGeneration:Boolean = true
    val verifyGoldPaths:Boolean = true

    println ("\n")
    println ("---------------------------")
    println ("Game Name: " + gameName)
    println ("Game Fold: " + gameFold)
    println ("---------------------------")

    val debugOutput:Boolean = false
    var actionsPerStep:Double = 0.0
    var numFailedGoldPaths:Int = 0

    // Use default game properties, unless overridden by input to this function
    //val properties = Map[String, Int]()
    //val properties = Map("numLocations" -> 11)
    //val properties = Map("numLocations" -> 3)

    val startTime = System.currentTimeMillis()

    val (success, gameGenerator) = GameGenerator.mkGameGenerator(gameName, properties)
    if (!success) {
      println ("ERROR creating Game Generator: ")
      println (gameGenerator.errorStr)
      sys.exit(1)
    }


    var numSteps:Long = 0
    for (i <- 0 until numEnvs) {

      var game:TextGame = null

      var goldPath = Array.empty[String]
      if (!goldPathGeneration) {
        game = gameGenerator.mkGame(seed = i, gameFold)
      } else {
        val (_game, _goldPath) = gameGenerator.mkGameWithGoldPath(seed = i, gameFold)
        game = _game
        goldPath = _goldPath

        if (verifyGoldPaths) {
          // TODO: Check for determinism -- run gold path, make sure it gives the same result.
          //println("Gold Path: " + goldPath.mkString(", "))
          // Initialize
          val initInfo = game.initalStep()
          // Run steps
          for (actionStr <- goldPath) {
            game.step(actionStr)
          }
          val scores = game.getScore()
          if (scores.scoreNormalized != 1.0) {
            println ("")
            println ("------------------------------------------------------------------------------------------------")
            println ("ERROR ON GAME " + i + " / " + numEnvs)
            println ("PLAYTHROUGH: (" + game.getHistory().length + " steps)")
            for (hStep <- game.getHistory()) {
              println ("> " + hStep.actionStr)
              println ("")
              println (hStep.observationStr)
              println ("Score: " + hStep.scores.scoreRaw)
            }
            println ("------------------------------------------------------------------------------------------------")
            //throw new RuntimeException("ERROR: Ran gold path, score not 1.0 (" + scores._2 + ")")
            numFailedGoldPaths += 1
          }

          // Reinitialize the game (either for the next gold agent attempt, or for the actual user agent attempt)
          game = gameGenerator.mkGame(seed=i, gameFold)
        }

      }





      if (debugOutput) println("\n-----------------------------------\n")

      var validActions:Array[String] = null
      val stepResultInitial = game.initalStep()
      validActions = stepResultInitial.validActions

      if (debugOutput) println("observationStr: " + stepResultInitial.observationStr)
      if (debugOutput) println("inventoryStr: " + stepResultInitial.inventoryStr)


      for (j <- 0 until numStepsPerEnv) {

        // Run action
          // Pick a random action
          val randActionIdx = Random.nextInt(validActions.length)
          if (debugOutput) {
            println(" > " + validActions(randActionIdx))
            println("")
          }

          val stepResult = game.step(randActionIdx)
          validActions = stepResult.validActions
          actionsPerStep += validActions.length

          if (debugOutput) {
            println("observationStr: " + stepResult.observationStr)
            println("freelookStr: " + stepResult.freeLookStr)
            println("inventoryStr: " + stepResult.inventoryStr)
            println ("Score: " + stepResult.scoreRaw + "   normalizedScore: " + stepResult.scoreNormalized + "  taskSuccess: " + stepResult.taskSuccess + "  taskFailure: " + stepResult.taskFailure)
          }

        numSteps += 1
      }

    }

    actionsPerStep = actionsPerStep / (numEnvs.toDouble * numStepsPerEnv.toDouble)

    val deltaTime = System.currentTimeMillis() - startTime
    println ("Environment: " + gameName)
    println ("Overridden properties: " + properties.toString())
    println ("Tested with: " + numEnvs + " generated environments, with " + numStepsPerEnv + " randomly chosen steps per environment.")
    println ("Delta time: " + deltaTime + " msec")
    println ("On average, each step has " + actionsPerStep.formatted("%3.3f") + " valid actions. ")
    println ("Rate: " + (deltaTime.toDouble / numEnvs.toDouble) + " msec/env")
    println ("Rate: " + (numEnvs.toDouble / (deltaTime.toDouble/1000.0f) ) + " envs/sec")

    println ("Rate: " + (numEnvs.toDouble * numStepsPerEnv.toDouble / (deltaTime.toDouble/1000.0f) ) + " steps/sec (including environment initialization)")

    println ("Number of failed gold paths: " + numFailedGoldPaths)

    println ("Total number of steps: " + numSteps)

    return numFailedGoldPaths
  }



  // Entry point
  def main(args:Array[String]): Unit = {

    val numEnvs:Int = 1000
    var numStepsPerEnv:Int = 50

    var totalErrors:Int = 0
    val errorSources = new ArrayBuffer[String]()

    val propsToTest = mutable.Map[String, ArrayBuffer[Map[String, Int]]]()
    // Add default properties case
    for (gameName <- GameGenerator.VALID_GAME_NAMES) {
      propsToTest(gameName) = new ArrayBuffer[Map[String, Int]]()
      propsToTest(gameName).append(Map[String, Int]()) // Blank properties are interpreted as default properties by the game generator
    }
    // Add any special cases/edge cases to test
    propsToTest("cookingworld").append( Map("numLocations" -> 3) )
    propsToTest("cookingworld").append( Map("numLocations" -> 1) )


    for (gameName <- GameGenerator.VALID_GAME_NAMES) {
      for (gameFold <- Array("train", "dev", "test")) {
        for (props <- propsToTest(gameName)) {
          val numErrors = this.testGame(gameName, gameFold, numEnvs, numStepsPerEnv, properties = props)

          if (numErrors > 0) {
            totalErrors += numErrors
            errorSources.append(gameName + " (" + gameFold + ") (Properties: " + props.toString() + ")")
          }
        }
      }
    }

    println ("")
    println ("---------------------------")
    println ("Total Errors: " + totalErrors)
    println ("Error Sources: " + errorSources.mkString(", "))
  }

}
