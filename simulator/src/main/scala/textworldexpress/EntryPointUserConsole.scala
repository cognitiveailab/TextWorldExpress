package textworldexpress

import textworldexpress.generator.GameGenerator

import scala.collection.mutable
import scala.util.Random

class EntryPointUserConsole {

}

object EntryPointUserConsole {

  def consoleTWC(): Unit = {
    //val SF_GAME_NAME:String = "twc"
    val SF_GAME_NAME:String = "cookingworld"
    //val SF_GAME_NAME:String = "mapreader"
    //val SF_GAME_NAME:String = "arithmetic"
    val seed = Random.nextInt(100)
    //val seed = 95
    val fold = "train"

    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    //gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 1                   // Number of locations
    //gameProps("maxDistanceApart") = 4               // Distance apart
    //gameProps("numIngredients") = 2
    //gameProps("numDistractorItems") = 10
    //gameProps("numItemsToPutAway") = 1              // Number of items to put away (TWC)


    val (success, generator) = GameGenerator.mkGameGenerator(gameName = SF_GAME_NAME, gameProps.toMap)

    val (game, goldPath) = generator.mkGameWithGoldPath(seed, fold)

    var count:Int = 0

    println ("Game: " + SF_GAME_NAME)
    println ("Fold: " + fold)
    println ("Seed: " + seed)
    println ("Gold path: " + goldPath.mkString(", "))
    println ("Game Properties: " + gameProps.toString())
    println ("Game Properties (from game): " + game.getGenerationProperties().toString())
    println ("-----------------------------------------------------------")
    println ("")

    var stepResult = game.initalStep()

    while (!stepResult.taskSuccess && !stepResult.taskFailure) {
      println ("Step: " + count)
      println ("Obs: " + stepResult.observationStr)
      println ("Valid actions: " + stepResult.validActions.mkString(", "))
      println ("Score: " + stepResult.scoreNormalized)

      val userInputStr = scala.io.StdIn.readLine("> ")

      stepResult = game.step(userInputStr)
      count += 1
    }

    println ("")
    println ("Game completed.")
    println ("Final score: " + stepResult.scoreNormalized)

  }



  def main(args:Array[String]) = {

    consoleTWC()

  }


}
