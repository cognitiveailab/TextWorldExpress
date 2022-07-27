package superfast.struct

import superfast.objects.FastObject

import scala.collection.mutable.ArrayBuffer

abstract class TextGame {


  /*
   * Cloning
   */
  def deepCopy():TextGame

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


