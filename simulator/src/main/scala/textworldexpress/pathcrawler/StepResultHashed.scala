package textworldexpress.pathcrawler

import textworldexpress.struct.StepResult

import scala.collection.mutable.ArrayBuffer

// Note: Storage class names are shortened, to reduce JSON size when serializing
case class StepResultHashed(val obs:Int, val look:Int, val inv:Int, val acts:Array[Int], val score:Double, val scoreNorm:Double, val succ:Boolean, val fail:Boolean, val valid:Boolean) {

  // Convert to StepResult
  def toStepResult(stringLUT:Array[String], randActionShuffleSeed:Int = -1): StepResult = {
    val validActionsStr = new Array[String](this.acts.length)
    for (i <- 0 until acts.length) {
      validActionsStr(i) = stringLUT(this.acts(i))
    }
    if (randActionShuffleSeed >= 0) {
      // TODO: Shuffle
    }

    try {
      val out = new StepResult(observationStr = stringLUT(obs),
        freeLookStr = stringLUT(look),
        inventoryStr = stringLUT(inv),
        validActions = validActionsStr,
        scoreRaw = score,
        scoreNormalized = scoreNorm,
        taskSuccess = succ,
        taskFailure = fail,
        wasValidAction = valid)

      return out
    } catch {
      case e:Throwable => {
        println("StringLUT: ")
        for (i <- 0 until stringLUT.length) {
          println(i + ":\t" + stringLUT(i))
        }
        println(e)

        System.exit(1)
        throw new RuntimeException("ERROR: " + e.toString)

      }
    }

  }

  // Helper function: Changes the score (used when extracting single-step paths from games)
  def toScore(score:Double):StepResultHashed = {
    return new StepResultHashed(obs=obs, look=look, inv=inv, acts=acts, score=score, scoreNorm=score, succ=false, fail=false, valid=true)
  }

}

object StepResultHashed {
  val stringLUT = new ArrayBuffer[String]()

  def resetLUT(): Unit = {
    stringLUT.clear()
  }

  private def add(strIn:String):Int = synchronized {
    val idx = this.stringLUT.indexOf(strIn)
    // Check for existing
    if (idx != -1) return idx
    // Does not exist -- add
    this.stringLUT.append(strIn)
    return this.stringLUT.length-1
  }

  def getStr(idx:Int):String = {
    if (idx < this.stringLUT.length) {
      return this.stringLUT(idx)
    }

    return "--UNDEFINED--"
  }

  def mkFromStepResult(in:StepResult):StepResultHashed = {
    val obsStrIdx = this.add(in.observationStr)
    val freeLookStrIdx = this.add(in.freeLookStr)
    val inventoryStrIdx = this.add(in.inventoryStr)

    val validActionsIdx = new Array[Int](in.validActions.length)
    for (i <- 0 until in.validActions.length) {
      validActionsIdx(i) = this.add( in.validActions(i) )
    }

    // Export
    val out = new StepResultHashed(obs = obsStrIdx, look = freeLookStrIdx, inv = inventoryStrIdx, acts = validActionsIdx, score = in.scoreRaw, scoreNorm = in.scoreNormalized, succ = in.taskSuccess, fail = in.taskFailure, valid = in.wasValidAction)
    return out
  }

  // Make a faux node with a given score
  def mkBlankWithScore(score:Double = 0.0, scoreNormalized:Double = 0.0, succ:Boolean = false, fail:Boolean = false):StepResultHashed = {
    val out = new StepResultHashed(obs = 0, look = 0, inv = 0, acts = Array.empty[Int], score = score, scoreNorm = scoreNormalized, succ = succ, fail = fail, valid = true)
    return out
  }

}

