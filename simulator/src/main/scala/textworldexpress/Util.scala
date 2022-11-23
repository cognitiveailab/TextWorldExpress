package textworldexpress

object JSON {
  def sanitize(in:String):String = {
    var out = in.replace("\\", "\\\\")
    out = out.replace("\"", "\\\"")
    out = out.replace("\n", "\\n")
    out = out.replace("\r", "\\r")
    out = out.replace("\t", "\\t")

    return out
  }
}