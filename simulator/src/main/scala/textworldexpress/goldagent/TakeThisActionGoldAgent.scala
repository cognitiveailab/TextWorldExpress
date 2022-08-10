package textworldexpress.goldagent

import textworldexpress.games.{ArithmeticGame, TakeThisActionGame}

import scala.util.Random

class TakeThisActionGoldAgent(game:TakeThisActionGame) {

  private def mkGoldPathTakeThisAction(r:Random): Boolean = {
    //game.step("look around")
    val stepResult = game.initalStep()

    // Step 1: read math problem
    for (i <- 0 until game.objectOrder.length) {
      game.step("read instructions")
      val obj = game.objectOrder(i)
      game.step("take " + obj.name)
    }

    val success = true

    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathTakeThisAction(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}


