package textworldexpress.struct


/*
 * Abstract class for implementations of Scorer
 */
abstract class Scorer {
  var curScore:GameScore = new GameScore()
  val maxScore:Double = this.calculateMaxScore()

  def getCurrentScore():GameScore = this.curScore

  def doScoring(): Unit
  def calculateMaxScore():Double
}
