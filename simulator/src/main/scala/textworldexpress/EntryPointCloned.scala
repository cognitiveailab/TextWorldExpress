package textworldexpress


import textworldexpress.games.{CoinGame, CoinGameGenerator, CookingWorldGame, CookingWorldGameGenerator, TWCGame, TWCGameGenerator}
import textworldexpress.generator.GameGenerator
import textworldexpress.goldagent.CookingWorldGoldAgent
import textworldexpress.objects.{Counter, FastObject, Fridge, Room}
import textworldexpress.struct.TextGame

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.control.Breaks._



object EntryPointCloned {


  def main(args:Array[String]): Unit = {
    //val startTime = System.currentTimeMillis()

    //val numEnvs:Int = 10000
    val numEnvs:Int = 100
    var numStepsPerEnv:Int = 50

    val verifyGoldPaths:Boolean = true
    //val verifyGoldPaths:Boolean = false

    val debugOutput:Boolean = false
    val console:Boolean = false
    if (console) numStepsPerEnv = 1000

    var actionsPerStep:Double = 0.0
    var numFailedGoldPaths:Int = 0

    val gameName = "kitchen"
    //val gameName = "twc"
    //val gameName = "coin"

    val gameFold:String = "train"
    //val properties = Map("numLocations" -> 11)
    val properties = Map("numLocations" -> 3)
    //val properties = Map("numLocations" -> 3, "numItemsToPutAway" -> 4)

    val (success, gameGenerator) = GameGenerator.mkGameGenerator(gameName, properties)
    if (!success) {
      println ("ERROR creating Game Generator: ")
      println (gameGenerator.errorStr)
      sys.exit(1)
    }

    // Pre-generate N games
    val numCachedGames:Int = 25
    val cachedGames = new ArrayBuffer[(TextGame, Array[String])]()

    for (i <- 0 until numCachedGames) {
      //cachedGames.append( gameGenerator.mkGame(seed=i, gameFold))
      val (_game, _goldPath) = gameGenerator.mkGameWithGoldPath(seed = i, gameFold)
      cachedGames.append( (_game, _goldPath) )
    }


    // Generate 'numEnvs'
    val startTime = System.currentTimeMillis()

    for (i <- 0 until numEnvs) {

      var game: TextGame = null

      var goldPath = Array.empty[String]

      // Get cached game
      val gameIdx = i % numCachedGames
      game = cachedGames(gameIdx)._1.deepCopy()
      goldPath = cachedGames(gameIdx)._2


      //if (!goldPathGeneration) {
      //game = gameGenerator.mkGame(seed = i, gameFold)
      //} else {
      //val (_game, _goldPath) = gameGenerator.mkGameWithGoldPath(seed = i, gameFold)
      //game = _game
      //goldPath = _goldPath

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
          println("ERROR ON GAME " + i + " / " + numEnvs)
          println("Score: " + scores.scoreNormalized)
          println("PLAYTHROUGH: (" + game.getHistory().length + " steps)")
          for (hStep <- game.getHistory()) {
            println("> " + hStep.actionStr)
            println("")
            println(hStep.observationStr)
            println("Score: " + hStep.scores.scoreRaw)
          }
          //throw new RuntimeException("ERROR: Ran gold path, score not 1.0 (" + scores._2 + ")")
          numFailedGoldPaths += 1
        }

        // Reinitialize the game (either for the next gold agent attempt, or for the actual user agent attempt)
        game = gameGenerator.mkGame(seed = i, gameFold)
      }

      //}


      //## clone test
      game match {
        case x: CoinGame => {
          //println ("Deep Copy")
          val clone = x.deepCopy()
        }
        case _ => {
          // Do nothing
        }
      }


      if (debugOutput) println("\n-----------------------------------\n")

      var validActions: Array[String] = null
      val stepResultInitial = game.initalStep()
      validActions = stepResultInitial.validActions

      if (debugOutput) println("observationStr: " + stepResultInitial.observationStr)
      if (debugOutput) println("inventoryStr: " + stepResultInitial.inventoryStr)
      /*
    println("----------------------------------------------")
    println("GOLD AGENT")

    for (i <- 0 until game.history.length) {
      println (i + ":")
      println ("> " + game.history(i).actionStr)
      println ("")
      println ("Obs: " + game.history(i).observationStr)
      println ("Score: " + game.history(i).score)
    }
    //sys.exit(1)
     */

      for (j <- 0 until numStepsPerEnv) {

        // Run action
        if (!console) {
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
            println("Score: " + stepResult.scoreRaw + "   normalizedScore: " + stepResult.scoreNormalized + "  taskSuccess: " + stepResult.taskSuccess + "  taskFailure: " + stepResult.taskFailure)
          }

        } else {
          // Console
          print("> ")

          val inputStr = readLine().trim().toLowerCase
          val stepResult = game.step(inputStr)
          validActions = stepResult.validActions

          println("")
          println("observationStr: " + stepResult.observationStr)
          println("freelookStr: " + stepResult.freeLookStr)
          println("inventoryStr: " + stepResult.inventoryStr)
          println("Score: " + stepResult.scoreRaw + "   normalizedScore: " + stepResult.scoreNormalized + "  taskSuccess: " + stepResult.taskSuccess + "  taskFailure: " + stepResult.taskFailure)
        }


      }

    }


    actionsPerStep = actionsPerStep / (numEnvs.toDouble * numStepsPerEnv.toDouble)

    val deltaTime = System.currentTimeMillis() - startTime
    println ("Tested with: " + numEnvs + " generated environments, with " + numStepsPerEnv + " randomly chosen steps per environment.")
    println ("Delta time: " + deltaTime + " msec")
    println ("On average, each step has " + actionsPerStep.formatted("%3.3f") + " valid actions. ")
    println ("Rate: " + (deltaTime.toDouble / numEnvs.toDouble) + " msec/env")
    println ("Rate: " + (numEnvs.toDouble / (deltaTime.toDouble/1000.0f) ) + " envs/sec")

    println ("Rate: " + (numEnvs.toDouble * numStepsPerEnv.toDouble / (deltaTime.toDouble/1000.0f) ) + " steps/sec (including environment initialization)")

    println ("Number of failed gold paths: " + numFailedGoldPaths)


  }

}

