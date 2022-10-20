package textworldexpress.symbolicmodule

import scala.collection.mutable.ArrayBuffer

class ModuleCalc(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {
  val validOperations = Array("add", "sub", "mul", "div")

  val validArguments = new ArrayBuffer[Int]()
  if (properties.contains("hidden_num1")) validArguments.append( properties("hidden_num1") )
  if (properties.contains("hidden_num2")) validArguments.append( properties("hidden_num2") )


  override def getValidCommands(): Array[String] = {
    // Check that there are exactly 2 arguments
    if (validArguments.length != 2) return Array.empty[String]

    val out = new ArrayBuffer[String]

    for (opStr <- this.validOperations) {
      out.append( opStr + " " + validArguments(0) + " " + validArguments(1))
      out.append( opStr + " " + validArguments(1) + " " + validArguments(0))
    }

    // Convert to a set to remove duplicates
    return out.toSet.toArray
  }


  override def runCommand(actionStr: String): String = {
    // Step 1: Make sure this is a valid command
    if (!this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Step 2: Split the action into the operation and arguments
    val fields = actionStr.split(" ")
    val opStr = fields(0)
    val arg1 = fields(1).toInt
    val arg2 = fields(2).toInt

    if (opStr == "add") {
      val result = arg1 + arg2
      return "The result of adding " + arg1 + " and " + arg2 + " is " + result
    } else if (opStr == "sub") {
      val result = arg1 - arg2
      return "The result of subtracting " + arg2 + " from " + arg1 + " is " + result
    } else if (opStr == "mul") {
      val result = arg1 * arg2
      return "The result of multiplying " + arg1 + " and " + arg2 + " is " + result
    } else if (opStr == "div") {
      val result = math.round(arg2.toFloat / arg1.toFloat).toInt
      /*
      val resultFloat = arg2.toFloat / arg1.toFloat
      // Show zero decimal places if an integer, and 1 if a float.
      var resultStr = ""
      if (result.toFloat == resultFloat) {
        // Integer
        resultStr = result.toString
      } else {
        // Float result
        resultStr = resultFloat.formatted("%3.1f")
      }

      return "The result of dividing " + arg2 + " by " + arg1 + " is " + resultStr
       */
      return "The result of dividing " + arg2 + " by " + arg1 + " is " + result     // Always round to int
    }

    // Default return
    return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
  }

}

object ModuleCalc {
  val MODULE_NAME   = "calc"
}