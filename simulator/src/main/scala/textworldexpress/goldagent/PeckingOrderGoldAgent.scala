package textworldexpress.goldagent

import textworldexpress.games.PeckingOrderGame

import scala.util.Random

class PeckingOrderGoldAgent(game:PeckingOrderGame) {

  private def mkGoldPathPeckingOrder(r:Random): Boolean = {
    val stepResult = game.initalStep()

    for (i <- 0 until game.objectOrder.length) {
      game.step("read instructions book")
      val obj = game.objectOrder(i)
      game.step("take " + obj.name)
    }

    val success = true

    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathPeckingOrder(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}
