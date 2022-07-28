package textworldexpress.struct

/*
 * Store class for score for a game at a given step
 */
case class GameScore(val scoreRaw:Double, val scoreNormalized:Double, val taskSuccess:Boolean, val taskFailure:Boolean) {
  def this() = this(0, 0, false, false)   // Empty constructor that initializes with zero score
}
