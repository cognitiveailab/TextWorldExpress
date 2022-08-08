package textworldexpress.goldagent

import textworldexpress.games.{ArithmeticGame, CoinGame}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class ArithmeticGoldAgent(game:ArithmeticGame) {

  private def mkGoldPathArithmetic(r:Random): Boolean = {
    //game.step("look around")
    val stepResult = game.initalStep()

    // Step 1: read math problem
    game.step("take math problem")
    game.step("read math problem")

    // Step 2: Pick up correct task object
    val correctObjName = game.correctObject
    game.step("take " + correctObjName.name)

    // Step 3: Put in answer box
    val answerBox = game.answerBox
    game.step("put " + correctObjName.name + " in " + answerBox.name)

    val success = true

    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathArithmetic(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}

