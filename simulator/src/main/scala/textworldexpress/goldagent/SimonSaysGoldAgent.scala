package textworldexpress.goldagent

import textworldexpress.games.SimonSaysGame

import scala.util.Random

class SimonSaysGoldAgent(game:SimonSaysGame) {

  private def mkGoldPathSimonSays(r:Random): Boolean = {
    val stepResult = game.initalStep()

    // Step 1: Perform each requested action, in sequence
    for (i <- 0 until game.goldActionSequence.length) {
      val nextActionStr = game.goldActionSequence(i)
      game.step(nextActionStr)
    }

    val success = true

    return success
  }

  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathSimonSays(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    val pathTrimmed = path.slice(1, path.length)
    return (true, pathTrimmed)
  }

}


