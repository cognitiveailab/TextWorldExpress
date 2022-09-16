package textworldexpress.goldagent

import textworldexpress.games.SortingGame
import textworldexpress.runtime.PythonInterface
import textworldexpress.symbolicmodule.ModuleSortByQuantity

import scala.util.Random


class SortingGoldAgent(game:SortingGame) {

  private def mkGoldPathSorting(r:Random): Boolean = {
    val stepResult = game.initalStep()

    // Step 1: Perform each requested action, in sequence
    for (i <- 0 until game.itemsToSort.length) {
      val objectName = game.itemsToSort(i).name
      game.step("take " + objectName)
      val boxName = game.answerBox.name
      game.step("put " + objectName + " in " + boxName)
    }

    val success = true

    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathSorting(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    val pathTrimmed = path.slice(1, path.length)
    return (true, pathTrimmed)
  }

}


/*
 * Agent with module support
 */
class SortingGoldAgentWithModule(interface:PythonInterface) {
  val game = interface.game match { case x:SortingGame => x; case _=> null }

  // Make the gold path, using the Calc module
  private def mkGoldPathSortingWithCalcModule(r:Random): Boolean = {
    // Ensure that we're running on the correct game type (ArithmeticGame)
    if (game == null) return false
    // Ensure that the appropriate modules have been enabled
    if (!interface.enabledModuleStrs.contains(ModuleSortByQuantity.MODULE_NAME)) {
      println ("SortingGoldAgentWithModule: ERROR: " + ModuleSortByQuantity.MODULE_NAME + " is not enabled -- can not generate gold path.")
    }

    // TODO: Retrieve the game mode (ascending versus descending)
    //val numArg1 = interface.game.getGenerationProperties()("hidden_num1")

    val stepResult = game.initalStep()

    // Step 1: Sort the observed objects
    interface.step("sort ascending")

    // Step 1: Perform each requested action, in sequence
    for (i <- 0 until game.itemsToSort.length) {
      val objectName = game.itemsToSort(i).name
      interface.step("take " + objectName)
      val boxName = game.answerBox.name
      interface.step("put " + objectName + " in " + boxName)
    }

    val success = true

    return success
  }

  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathSortingWithCalcModule(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = interface.history.map(_.userInputStr).toArray.filter(_.length > 0)
    return (true, path)
  }

}