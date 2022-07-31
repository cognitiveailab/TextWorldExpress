package textworldexpress.benchmark

import textworldexpress.pathcrawler.{CrawlerRunner, PrecrawledPath}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

class BenchmarkPrecrawledPathThreaded {

}

/*
 * Benchmarks traversal speed of a random agent using the precrawled paths.
 */

// The random agent thread
class RandomAgentThreadRunner(id:Int, precrawledPaths:Array[PrecrawledPath], numEpisodesToRun:Int=100000) extends Thread {
  private var isRunning:Boolean = false
  private var isWinning:Boolean = false
  private var isCompleted:Boolean = false

  private val verboseDebugOutput:Boolean = false

  private var totalSteps:Long = 0
  private var totalEpisodes:Int = 0

  // Set thread ID
  //this.setName(id.toString)

  //val crawler = new EntryPointPathCrawler(SF_GAME_NAME, gameProps, startSeed, gameFold)

  def isThreadRunning():Boolean = return this.isRunning

  def getTotalStepsRun():Long = return this.totalSteps
  def getTotalEpisodesRun():Int = return this.totalEpisodes


  override def run(): Unit = {
    this.isRunning = true
    if (verboseDebugOutput) println("Thread " + Thread.currentThread().getName() + " is running.")

    for (i <- 0 to numEpisodesToRun) {
      val precrawledPath = precrawledPaths(i % precrawledPaths.length)
      this.totalSteps += randomAgentPrecrawled(precrawledPath, verbose = false)
      this.totalEpisodes += 1
    }

    if (verboseDebugOutput) println("Thread " + Thread.currentThread().getName() + " is completed.")
    this.isRunning = false
    this.isCompleted = true
  }


  // Randomly crawl a precrawled path.
  private def randomAgentPrecrawled(precrawledPath:PrecrawledPath, maxSteps:Int = 50, verbose:Boolean = false): Int = {
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


}


object BenchmarkPrecrawledPathThreaded {

  def printUsage(): Unit = {
    println ("Usage: BenchmarkPrecrawledPathThreaded <numThreads>")
  }

  def main(args:Array[String]): Unit = {
    val verboseOutput:Boolean = false
    var numThreads:Int = 32
    val numEpsiodesToRun:Int = 1000000

    if (args.length == 0) {
      this.printUsage()
      println ("Missing argument defining number of threads to run.  Defaulting to 32.")

    } else if (args.length == 1) {
      try {
        numThreads = args(0).toInt
      } catch {
        case _:Throwable => {
          println("Unable to parse (" + args(0) + ") into <numThreads> argument.  Expected integer. ")
          this.printUsage()
        }
      }

    } else {
      this.printUsage()
      sys.exit(1)
    }

    // Step 1: Load precrawled paths
    val numPerFold = 5
    val precrawledPaths = new Array[PrecrawledPath](numPerFold)
    for (i <- 0 until numPerFold) {
      val path = "/home/peter/github/sciworld-synth/"
      //val filename = path + "savetest-gametwc-var" + i + "-foldtrain-maxDepth6-includeDoors0-numItemsToPutAway1-numLocations1.json"
      val filename = path + "savetest-gamecoin-var" + i + "-foldtrain-maxDepth10-includeDoors0-limitInventorySize0-numDistractorItems0-numLocations4.json"
      val precrawledPath = PrecrawledPath.loadFromJSON(filename).get
      precrawledPaths(i) = precrawledPath
    }

    println ("Starting Random Agents")

    val startTime = System.currentTimeMillis()

    var totalSteps:Long = 0
    var totalEpisodes:Int = 0

    // Create array of thread runners
    val runners = new Array[RandomAgentThreadRunner](numThreads)

    // Start threads
    println ("Starting " + numThreads + " threads... ")
    for (i <- 0 until numThreads) {
      runners(i) = new RandomAgentThreadRunner(i, precrawledPaths, numEpsiodesToRun)
      runners(i).start()
    }

    // Wait for threads to finish
    var stillRunning = new ArrayBuffer[Int]()     // More elegant ways of doing this
    stillRunning.append(-1)  // Initial

    while (stillRunning.length > 0) {
      stillRunning.clear()

      for (i <- 0 until numThreads) {
        if (runners(i).isThreadRunning()) {
          stillRunning.append(i)
        }
      }

      Thread.sleep(250)
      val deltaTime = ((System.currentTimeMillis() - startTime).toDouble/ 1000)   // In seconds
      println("(" + math.round(deltaTime) + " sec) Threads still running: " + stillRunning.mkString(" "))
    }

    // Get results from threads (total number of steps)
    for (i <- 0 until numThreads) {
      totalSteps += runners(i).getTotalStepsRun()
      totalEpisodes += runners(i).getTotalEpisodesRun()
    }

    val deltaTime = ((System.currentTimeMillis() - startTime).toDouble/ 1000)   // In seconds

    println ("Number of threads : " + numThreads)
    println ("Total Episodes    : " + totalEpisodes)
    println ("Total Steps       : " + totalSteps)
    println ("Total Time        : " + deltaTime + " sec")

    val rate = totalSteps.toDouble / deltaTime.toDouble
    println ("Rate              : " + rate.formatted("%3.3f") + " steps/sec")

    val timeTo1BSteps = 1000000000.0 / rate
    println ("Time to 1B steps  : " + timeTo1BSteps.formatted("%3.3f") + " seconds")

  }

}