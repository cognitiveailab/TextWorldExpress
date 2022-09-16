package textworldexpress.goldagent

import textworldexpress.games.{ArithmeticGame, CoinGame}
import textworldexpress.runtime.PythonInterface
import textworldexpress.symbolicmodule.ModuleCalc

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

  // Make the gold path, using the Calc module
  private def mkGoldPathArithmeticWithCalcModule(r:Random): Boolean = {
    //game.step("look around")
    var stepResult = game.initalStep()

    // Step 1: read math problem
    game.step("take math problem")
    stepResult = game.step("read math problem")

    // Choose the correct calculator action
    val numArg1 = game.generationProperties("hidden_num1")
    val numArg2 = game.generationProperties("hidden_num2")
    val operation = game.generationProperties("hidden_op")
    if (operation == 0) {
      game.step("add " + numArg1 + " " + numArg2)
    } else if (operation == 1) {
      game.step("sub " + numArg1 + " " + numArg2)
    } else if (operation == 2) {
      game.step("mul " + numArg1 + " " + numArg2)
    } else if (operation == 3) {
      game.step("div " + numArg1 + " " + numArg2)
    } else {
      println ("Unknown operation (" + operation + ") -- This should never happen.")
      game.step("error: unknown operation")
      return false
    }

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
    var success = this.mkGoldPathArithmetic(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}


/*
 * Agent with module support
 */
class ArithmeticGoldAgentWithModule(interface:PythonInterface) {
  val game = interface.game match { case x:ArithmeticGame => x; case _=> null }

  // Make the gold path, using the Calc module
  private def mkGoldPathArithmeticWithCalcModule(r:Random): Boolean = {
    // Ensure that we're running on the correct game type (ArithmeticGame)
    if (game == null) return false
    // Ensure that the appropriate modules have been enabled
    if (!interface.enabledModuleStrs.contains(ModuleCalc.MODULE_NAME)) {
      println ("ArithmeticGoldAgentWithModule: ERROR: " + ModuleCalc.MODULE_NAME + " is not enabled -- can not generate gold path.")
    }


    // Step 1: read math problem
    interface.step("take math problem")
    interface.step("read math problem")

    // Choose the correct calculator action
    val numArg1 = interface.game.getGenerationProperties()("hidden_num1")
    val numArg2 = interface.game.getGenerationProperties()("hidden_num2")
    val operation = interface.game.getGenerationProperties()("hidden_op")
    var opStr:String = "ERROR: Unknown operation"
    if (operation == 0) {
      opStr = "add " + numArg1 + " " + numArg2
    } else if (operation == 1) {
      opStr = "sub " + numArg1 + " " + numArg2
    } else if (operation == 2) {
      opStr = "mul " + numArg1 + " " + numArg2
    } else if (operation == 3) {
      opStr = "div " + numArg1 + " " + numArg2
    } else {
      println ("Unknown operation (" + operation + ") -- This should never happen.")
      return false
    }
    interface.step(opStr)

    // Step 2: Pick up correct task object
    val correctObjName = game.correctObject
    interface.step("take " + correctObjName.name)

    // Step 3: Put in answer box
    val answerBox = game.answerBox
    interface.step("put " + correctObjName.name + " in " + answerBox.name)

    val success = true

    return success
  }

  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathArithmeticWithCalcModule(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = interface.history.map(_.userInputStr).toArray.filter(_.length > 0)
    return (true, path)
  }

}
