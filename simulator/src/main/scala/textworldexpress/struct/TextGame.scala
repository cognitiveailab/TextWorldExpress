package textworldexpress.struct

import textworldexpress.objects.FastObject

import scala.collection.mutable.ArrayBuffer

abstract class TextGame {


  /*
   * Cloning
   */
  def deepCopy():TextGame

  /*
   * Properties
   */

  def getGenerationProperties():Map[String, Int]

  /*
   * History
   */
  def getHistory():ArrayBuffer[ActionHistory]

  /*
   * Scoring
   */
  def getScore():GameScore

  /*
   * Steps
   */

  def initalStep():StepResult

  def step(actionStr:String):StepResult

  def step(validActionIdx:Int):StepResult

  def step(actionStr: String, actionNumber: Int, actionParams: Array[FastObject]):StepResult

}


