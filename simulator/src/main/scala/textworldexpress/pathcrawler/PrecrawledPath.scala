package textworldexpress.pathcrawler

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import textworldexpress.struct.StepResult

import scala.collection.immutable.HashMap

case class PrecrawledPath(val nodeLUT:Array[PrecrawledNode], val stringLUT:Array[String]) {
  val stringToIDXLUT = this.mkStringToIDXLUT()

  def sizeNodes():Int = nodeLUT.length
  def sizeStrings():Int = stringLUT.length

  /*
   * Accessors
   */
  def getNode(nodeIdx:Int):PrecrawledNode = {
    return this.nodeLUT(nodeIdx)
  }

  def getStartNode():PrecrawledNode = {
    return this.getNode(0)
  }

  /*
   * Take action
   */
  def takeAction(curNode:PrecrawledNode, requestedActionStr:String):Option[PrecrawledNode] = {
    // Step 1: Find the string LUT index of the requested action string
    val idx = stringToIDXLUT.getOrElse(requestedActionStr, -1)
    if (idx == -1) return None                        // If string wasn't found, exit
    if (!curNode.steps.contains(idx)) return None     // If string isn't a valid transition from this node, exit

    // Step 2: Take action
    val nextNodeIdx = curNode.steps(idx)
    val nextNode = this.nodeLUT(nextNodeIdx)

    return Some(nextNode)
  }

  /*
   * LUT helpers
   */
  def mkStringToIDXLUT():Map[String, Int] = {
    val out = mutable.Map[String, Int]()

    for (i <- 0 until this.stringLUT.length) {
      out(this.stringLUT(i)) = i
    }

    return out.toMap
  }

  /*
   * Finding winning paths
   */
  def findWinningPaths(curNode:PrecrawledNode = this.getStartNode(), lastActionStr:String = "look around"):Array[Array[GoldPathStep]] = {

    // Winning case: Found winning path
    if (curNode.result.succ) {
      return Array(Array( new GoldPathStep(lastActionStr, curNode.result.toStepResult(this.stringLUT), curNode) ))
    }

    // Stop case: Found failing path
    if (curNode.result.fail) return Array.empty[Array[GoldPathStep]]

    // Recursive case: For each valid action from this step, explore it's subtree for winning paths
    val out = new ArrayBuffer[Array[GoldPathStep]]

    val validActions = curNode.steps    // (StringLUT -> NodeIdx)
    for (validActionIdx <- validActions.keySet) {
      val validActionStr = this.stringLUT(validActionIdx)
      val nextNodeIdx = validActions(validActionIdx)
      val nextNode = this.getNode(nextNodeIdx)

      val winningPathsIn = this.findWinningPaths(curNode = nextNode, lastActionStr = validActionStr)
      if (winningPathsIn.length > 0) {
        for (inPath <- winningPathsIn) {
          out.append( Array(new GoldPathStep(lastActionStr, curNode.result.toStepResult(this.stringLUT), curNode)) ++ inPath)
        }
      }
    }

    // Return
    return out.toArray
  }





  /*
   * JSON saving
   */
  def exportToJSONStr():String = {
    System.gc()
    return this.asJson.noSpaces
  }

  def saveToJSON(filename:String) = {
    val pw = new PrintWriter(filename)
    pw.print(this.exportToJSONStr())
    pw.close()
  }

  /*
   * String methods
   */
  def StringLUTToString():String = {
    val os = new mutable.StringBuilder()

    for (i <- 0 until stringLUT.length) {
      os.append(i + ": \t" + stringLUT(i) + "\n")
    }
    os.toString()
  }
}


object PrecrawledPath {

  /*
   * Loading
   */

  def loadFromJSON(filename:String):Option[PrecrawledPath] = {
    println (" * Loading precrawled path (" + filename + ").")
    val fileContentsStr = scala.io.Source.fromFile(filename).mkString
    val decoded = decode[PrecrawledPath](fileContentsStr).toOption
    if (decoded.isDefined) {
      println (" * Successfully loaded (" + decoded.get.sizeNodes() + " nodes, " + decoded.get.sizeStrings() + " strings).")
    } else {
      println (" * ERROR: Unable to load path.")
    }

    return decoded
  }


  /*
   * Creation
   */

  def make(root:PrecrawledPathNode, stringLUT:ArrayBuffer[String]): PrecrawledPath = {
    // Step 1: First, label the path nodes with unique sequential IDs
    PrecrawledPathNode.assignUniqueIDs(root)

    // Step 2: Create an array to use as a fast look-up-table for these nodes
    val numNodes = PrecrawledPathNode.curId + 1
    val nodeLUT = new Array[PrecrawledNode](numNodes)

    // Step 3: Populate the nodes
    this.populateNodeLUT(root, nodeLUT, stringLUT)

    // Step 4: Generate storage class
    val out = new PrecrawledPath(nodeLUT = nodeLUT, stringLUT = stringLUT.toArray)

    return out
  }


  // Recursively populate all the nodes into the node look-up table (converting them to a different storage class in the process)
  private def populateNodeLUT(in:PrecrawledPathNode, nodeLUT:Array[PrecrawledNode], stringLUT:ArrayBuffer[String]): Unit = {
    // Convert this node
    val converted = PrecrawledNode.mkFromRaw(in, stringLUT)

    // Store this node
    val nodeIdx = in.id
    nodeLUT(nodeIdx) = converted

    // Recurse
    for (child <- in.validSteps.values) {
      this.populateNodeLUT(child, nodeLUT, stringLUT)
    }
  }

}



// One hashed node in a precrawled tree
case class PrecrawledNode(val result:StepResultHashed, val steps:Map[Int, Int]) {

  // Helper used for converting long paths into single-step games.
  def convertToSingleStep(winningStepHashIdx:Int, idxOfWinningNode:Int, idxOfFailingNode:Int): PrecrawledNode = {
    // Step 1: Create new alternate 'next steps' map.
    val newSteps = mutable.Map[Int, Int]()
    // First, set all next-actions from this node to go to the failing condition node by default
    for (idx <- steps.keySet) {
      newSteps(idx) = idxOfFailingNode
    }
    // Then, set the one winning step to go to the winning node
    newSteps(winningStepHashIdx) = idxOfWinningNode

    // Step 2: Remake the node
    val out = new PrecrawledNode(result.toScore(0.0), newSteps.toMap)        // NOTE: Also sets the node's score to zero.
    return out
  }

}

object PrecrawledNode {

  def mkFromRaw(in:PrecrawledPathNode, stringLUT:ArrayBuffer[String]):PrecrawledNode = {
    val stepResult = in.stepResult

    // Convert valid steps to hashes
    val validSteps = mutable.Map[Int, Int]()
    for (step <- in.validSteps) {
      val actionStr = step._1
      val resultingNode = step._2

      val actionStrIdx = stringLUT.indexOf(actionStr)
      val nodeIdx = resultingNode.id
      validSteps(actionStrIdx) = nodeIdx
    }

    val out = new PrecrawledNode(result = stepResult, steps = validSteps.toMap)
    return out
  }

}


// One step in a gold/winning path
case class GoldPathStep(val actionStr:String, val result:StepResult, val node:PrecrawledNode) {

}




object Test {

  def getWinningPathsCoin(startIdx:Int=0, endIdx:Int=20): Array[ Array[Array[GoldPathStep]] ] = {
    val out = new ArrayBuffer[ Array[Array[GoldPathStep]] ]   // (Game Variation, Different Winning Paths, Action Sequences)

    for (i <- startIdx until endIdx) {
      val filename = "savetest-gamecoin-var" + i + "-foldtrain-maxDepth10-includeDoors0-limitInventorySize0-numDistractorItems0-numLocations3.json"
      val precrawledPath = PrecrawledPath.loadFromJSON(filename).get


      // Find winning paths
      var winningPaths = precrawledPath.findWinningPaths()
      winningPaths = winningPaths.sortBy(_.length)

      out.append(winningPaths)
    }

    println(" * Loaded precrawled paths for " + (endIdx - startIdx) + " variations.")

    return out.toArray
  }

  def getWinningPathsTWC(startIdx:Int=0, endIdx:Int=20, numItemsToPutAway:Int=1): Array[ Array[Array[GoldPathStep]] ] = {
    val out = new ArrayBuffer[ Array[Array[GoldPathStep]] ]   // (Game Variation, Different Winning Paths, Action Sequences)

    for (i <- startIdx until endIdx) {
      val filename = "savetest-gametwc-var" + i + "-foldtrain-maxDepth7-includeDoors0-numItemsToPutAway" + numItemsToPutAway + "-numLocations1.json"
      val precrawledPath = PrecrawledPath.loadFromJSON(filename).get


      // Find winning paths
      var winningPaths = precrawledPath.findWinningPaths()
      winningPaths = winningPaths.sortBy(_.length)

      out.append(winningPaths)
    }

    println(" * Loaded precrawled paths for " + (endIdx - startIdx) + " variations.")

    return out.toArray
  }



  def main(args:Array[String]) = {

    //val winningPathsCoin = this.getWinningPathsCoin(startIdx = 0, endIdx = 20)

    val winningPathsTWC = this.getWinningPathsTWC(startIdx = 0, endIdx = 20, numItemsToPutAway = 2)

    for (i <- 0 until winningPathsTWC.length) {
      val winningPaths = winningPathsTWC(i).sortBy(_.length)

      println ("Winning Paths (variation " + i + ", number of winning paths: " + winningPaths.length + "):")
      for (j <- 0 until math.min(10, winningPaths.length)) {
        println(j + ": \t" + winningPaths(j).length + "\t" + winningPaths(j).map(_.actionStr).mkString(", ") )
      }

      println ("")


    }

  }


}
