package textworldexpress.knowledgebase

class KBResult(val results:Array[Array[String]]) {
  var curIdx:Int = 0
  var maxIdx:Int = results.length

  // Copies results, but with a fresh 'curIdx' (i.e. uniterated/unused)
  def copy():KBResult = {
    // TODO: It's probably OK that the 'results' are a shallow copy.
    val out = new KBResult(results.clone())
    return out
  }

  /*
   * Accessors
   */

  // Reset iterator to start
  def reset(): Unit = {
    this.curIdx = 0
  }

  // Check if there are more things to iterate over
  def hasNext():Boolean = {
    if (curIdx < maxIdx) {
      return true
    }
    return false
  }

  // Get the current item in the iterator
  def getNext():Array[String] = {
    curIdx += 1
    return results(curIdx-1)
  }

  def getCurrent():Array[String] = {
    return results(curIdx)
  }

  def length:Int = {
    return this.maxIdx
  }

  /*
   * String methods
   */
  override def toString():String = {
    results.mkString("\t")
  }
}
