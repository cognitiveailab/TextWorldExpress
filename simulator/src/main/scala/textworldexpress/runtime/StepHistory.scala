package textworldexpress.runtime

import textworldexpress.struct.StepResult

/*
 * Storage class for one step of a text game history
 */
class StepHistory(val actionStr:String, val moduleStr:String, val stepResult:StepResult) {
  val userInputStr = if (moduleStr.length > 0) { this.moduleStr } else { this.actionStr }

}
