package textworldexpress.runtime

import textworldexpress.struct.StepResult

/*
 * Storage class for one step of a text game history
 */
class StepHistory(val actionStr:String, val moduleStr:String, val stepResult:StepResult) {
  val userInputStr = if (moduleStr.length > 0) { this.moduleStr } else { this.actionStr }

  def toJSON():String = {
    val os = new StringBuilder()
    os.append("{")
    os.append("\"actionStr\":\"" + sanitizeJSON(actionStr) + "\",")
    os.append("\"moduleStr\":\"" + sanitizeJSON(moduleStr) + "\",")
    os.append("\"stepResult\": " + stepResult.toJSON() + " ")
    os.append("}")

    os.toString()
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
