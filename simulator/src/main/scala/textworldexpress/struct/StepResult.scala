package textworldexpress.struct

/*
 * Storage class for the result of an environment step
 */
class StepResult(val observationStr:String, val freeLookStr:String, val inventoryStr:String, val validActions:Array[String], val scoreRaw:Double, val scoreNormalized:Double, val taskSuccess:Boolean, val taskFailure:Boolean, val wasValidAction:Boolean) {

}

object StepResult {

  // Make a faux invalid action.
  def mkInvalidStep(in:StepResult):StepResult = {
    return new StepResult(
      observationStr = "I'm not sure what you mean.",
      freeLookStr = in.freeLookStr,
      inventoryStr = in.inventoryStr,
      validActions = in.validActions,
      scoreRaw = in.scoreRaw,
      scoreNormalized = in.scoreNormalized,
      taskSuccess = in.taskSuccess,
      taskFailure = in.taskFailure,
      wasValidAction = in.wasValidAction
    )

  }
}