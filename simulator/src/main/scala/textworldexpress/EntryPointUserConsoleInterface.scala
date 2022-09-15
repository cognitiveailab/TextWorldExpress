package textworldexpress

import textworldexpress.generator.GameGenerator
import textworldexpress.runtime.PythonInterface
import textworldexpress.symbolicmodule.{ModuleCalc, ModuleKnowledgeBaseTWC}

import collection.JavaConverters._
import scala.collection.mutable

class EntryPointUserConsoleInterface {

}

object EntryPointUserConsoleInterface {

  def consoleTWC(): Unit = {
    val interface = new PythonInterface()


    //val SF_GAME_NAME:String = "twc"
    //val SF_GAME_NAME:String = "cookingworld"
    //val SF_GAME_NAME:String = "mapreader"
    //val SF_GAME_NAME:String = "mapreader-random"
    val SF_GAME_NAME:String = "arithmetic"
    //val SF_GAME_NAME:String = "takethisaction"
    //val SF_GAME_NAME:String = "simonsays"
    //val SF_GAME_NAME:String = "sorting"
    //val SF_GAME_NAME:String = "simonsays-memory"
    //val seed = Random.nextInt(100)
    val seed = 85
    val fold = "train"

    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    //gameProps("includeDoors") = 0                   // Disable doors
    //gameProps("numLocations") = 1                   // Number of locations
    //gameProps("maxDistanceApart") = 4               // Distance apart
    //gameProps("numIngredients") = 2
    //gameProps("numDistractorItems") = 10
    //gameProps("numItemsToPutAway") = 1              // Number of items to put away (TWC)

    val paramStr = ""
    //val paramStr = "numLocations=1"
    //val enabledModules = ""
    val enabledModules = ModuleCalc.MODULE_NAME
    //val enabledModules = ModuleKnowledgeBaseTWC.MODULE_NAME

    interface.load(gameName = SF_GAME_NAME, gameFold = fold, seed = seed, paramStr = paramStr, generateGoldPath = true, enabledModulesStr = enabledModules)
    var stepResult = interface.generateNewGame(seed = seed, gameFold = fold, generateGoldPath = true)

    val game = interface.game
    val goldPath = interface.getGoldActionSequence().asScala.toArray

    var count:Int = 0

    println ("Game: " + SF_GAME_NAME)
    println ("Fold: " + fold)
    println ("Seed: " + seed)
    println ("Gold path: " + goldPath.mkString(", "))
    println ("Game Properties: " + gameProps.toString())
    println ("Game Properties (from game): " + interface.game.getGenerationProperties().toString())
    if (interface.moduleInterface != null) {
      println("Enabled Modules: " + interface.moduleInterface.getEnabledModuleNames().mkString(", "))
    }
    println ("-----------------------------------------------------------")
    println ("")
    println ("Task: " + game.getTaskDescription())
    println ("")

    //var stepResult = game.initalStep()

    while (!stepResult.taskSuccess && !stepResult.taskFailure) {
      println ("Step: " + count)
      println ("Obs: " + stepResult.observationStr)
      println ("Valid actions: " + stepResult.validActions.mkString(", "))
      println ("Score: " + stepResult.scoreNormalized)

      val userInputStr = scala.io.StdIn.readLine("> ")

      stepResult = interface.step(userInputStr)
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
