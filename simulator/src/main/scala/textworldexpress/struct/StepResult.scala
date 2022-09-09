package textworldexpress.struct

/*
 * Storage class for the result of an environment step
 */
class StepResult(val observationStr:String, val freeLookStr:String, val inventoryStr:String, val validActions:Array[String], val scoreRaw:Double, val scoreNormalized:Double, val taskSuccess:Boolean, val taskFailure:Boolean, val wasValidAction:Boolean) {

  def toJSON():String = {
    val os = new StringBuilder()
    os.append("{")
    os.append("\"observation\":\"" + sanitizeJSON(observationStr) + "\",")
    os.append("\"look\":\"" + sanitizeJSON(freeLookStr) + "\",")
    os.append("\"inventory\":\"" + sanitizeJSON(inventoryStr) + "\",")
    os.append("\"validActions\":[\"" + validActions.mkString("\",\"") + "\"],")
    os.append("\"scoreRaw\":" + scoreRaw + ",")
    os.append("\"score\":" + scoreNormalized + ",")
    os.append("\"tasksuccess\":\"" + taskSuccess + "\",")
    os.append("\"taskfailure\":\"" + taskFailure + "\"")
    os.append("}")

    os.toString()
  }

  // Clone this StepResult, but replace the observation with an externally supplied observation.
  // This is useful for the Symbolic Modules, that generally output only the observation.
  def cloneButReplaceObservation(newObservationStr:String): StepResult = {
    return new StepResult(
      observationStr = newObservationStr,
      freeLookStr = freeLookStr,
      inventoryStr = inventoryStr,
      validActions = validActions,
      scoreRaw = scoreRaw,
      scoreNormalized = scoreNormalized,
      taskSuccess = taskSuccess,
      taskFailure = taskFailure,
      wasValidAction = wasValidAction
    )
  }

  // Clone, but replace the valid actions.
  // This is useful for the symbolic modules, that add new valid actions to the action space.
  def cloneButReplaceValidActions(newValidActions:Array[String]): StepResult = {
    return new StepResult(
      observationStr = observationStr,
      freeLookStr = freeLookStr,
      inventoryStr = inventoryStr,
      validActions = newValidActions,
      scoreRaw = scoreRaw,
      scoreNormalized = scoreNormalized,
      taskSuccess = taskSuccess,
      taskFailure = taskFailure,
      wasValidAction = wasValidAction
    )
  }

  def sanitizeJSON(in:String):String = {
    var out = in.replace("\"", "\\\"")
    out = out.replace("\\", "\\\\")
    out = out.replace("\n", "\\n")
    out = out.replace("\r", "\\r")
    out = out.replace("\t", "\\t")

    return out
  }


}

object StepResult {

  // Make a faux invalid action.
  def mkInvalidStep(in:StepResult):StepResult = {
    return new StepResult(
      observationStr = "Unknown action: I'm not sure what you mean.",
      freeLookStr = in.freeLookStr,
      inventoryStr = in.inventoryStr,
      validActions = in.validActions,
      scoreRaw = in.scoreRaw,
      scoreNormalized = in.scoreNormalized,
      taskSuccess = in.taskSuccess,
      taskFailure = in.taskFailure,
      wasValidAction = false
    )
  }

  // Make error message
  def mkErrorMessage(errorStr:String):StepResult = {
    return new StepResult(
      observationStr = errorStr,
      freeLookStr = errorStr,
      inventoryStr = errorStr,
      validActions = Array.empty[String],
      scoreRaw = -1,
      scoreNormalized = -1,
      taskSuccess = false,
      taskFailure = true,
      wasValidAction = false
    )
  }

}