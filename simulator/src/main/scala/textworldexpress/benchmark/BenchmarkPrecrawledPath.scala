package textworldexpress.benchmark

import textworldexpress.pathcrawler.PrecrawledPath

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

/*
 * Benchmarks traversal speed of a random agent using the precrawled paths.
 */

object BenchmarkPrecrawledPath {

  // Randomly crawl a precrawled path.
  def randomAgentPrecrawled(precrawledPath:PrecrawledPath, maxSteps:Int = 50, verbose:Boolean = false): Int = {
    // Step 1: Get start node
    var curNode = precrawledPath.getStartNode()

    var curSteps:Int = 0
    breakable {
      while (curSteps < maxSteps) {
        // Check if we're at the end of the precrawled path
        if (curNode.steps.size == 0) {
          // No more valid actions -- likely we crawled to the end of the path.
          break()
        }

        // Step 2: Convert current node from hashed version to strings
        val curNodeStrs = curNode.result.toStepResult(precrawledPath.stringLUT)

        // Step 3: Pick random action
        val validActionsStrs = curNodeStrs.validActions
        val randIdx = Random.nextInt(validActionsStrs.length)
        val randomAction = validActionsStrs(randIdx)

        if (verbose) {
          println (curNode.toString)

          println ("Obs: " + curNodeStrs.observationStr)
          println ("Score: " + curNodeStrs.scoreNormalized)
          println ("Valid Actions: " + validActionsStrs.mkString(", "))
          println ("Random Action: " + randomAction)
          println ("")

          println("-------------------------------------------------------------")
        }

        // Step 4: Take action, traverse to next node
        val actionLUTIdx = precrawledPath.stringToIDXLUT(randomAction)
        val nodeToMoveTo = curNode.steps(actionLUTIdx)
        curNode = precrawledPath.getNode(nodeToMoveTo)

        curSteps += 1
      }

    }

    // Return the number of steps traversed
    return curSteps

  }


  def main(args:Array[String]): Unit = {
    val verboseOutput:Boolean = false

    // Step 1: Load precrawled paths
    val numPerFold = 5
    val precrawledPaths = new ArrayBuffer[PrecrawledPath]
    for (i <- 0 until numPerFold) {
      //val path = "/home/peter/github/sciworld-synth/"
      //val filename = path + "savetest-gametwc-var" + i + "-foldtrain-maxDepth6-includeDoors0-numItemsToPutAway1-numLocations1.json"
      val path = "precrawledpaths/"
      val filename = path + "precrawled-gamecoin-var" + i + "-foldtrain-maxDepth10-includeDoors0-limitInventorySize0-numDistractorItems0-numLocations4.json"
      val precrawledPath = PrecrawledPath.loadFromJSON(filename).get
      precrawledPaths.append(precrawledPath)
    }

    println ("Starting Random Agents")

    val startTime = System.currentTimeMillis()

    var totalSteps:Long = 0
    var totalEpisodes:Int = 0

    for (i <- 0 to 100000) {
      for (precrawledPath <- precrawledPaths) {
        totalSteps += randomAgentPrecrawled(precrawledPath, verbose = verboseOutput)
        totalEpisodes += 1
      }
    }


    val deltaTime = ((System.currentTimeMillis() - startTime).toDouble/ 1000)   // In seconds

    println ("Total Episodes    : " + totalEpisodes)
    println ("Total Steps       : " + totalSteps)
    println ("Total Time        : " + deltaTime + " sec")

    val rate = totalSteps.toDouble / deltaTime.toDouble
    println ("Rate              : " + rate.formatted("%3.3f") + " steps/sec")

  }

}