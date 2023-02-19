package textworldexpress.goldagent

import textworldexpress.games.{SortingGame}

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
