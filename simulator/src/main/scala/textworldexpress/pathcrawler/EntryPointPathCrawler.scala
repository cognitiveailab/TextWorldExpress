package textworldexpress.pathcrawler

import textworldexpress.generator.GameGenerator
import textworldexpress.struct.{StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//class CrawlerRunner(id:Int, SF_GAME_NAME:String = "coin", gameProps:Map[String, Int], initialPath:Array[String], startSeed:Int=0, gameFold:String="train", maxDepth:Int=5) extends Thread {
class CrawlerRunner(id:Int, crawler:EntryPointPathCrawler, initialPath:Array[String], maxDepth:Int=5) extends Thread {
  private var isRunning:Boolean = false
  private var isWinning:Boolean = false
  private var isCompleted:Boolean = false

  private val verboseDebugOutput:Boolean = true

  // Set thread ID
  //this.setName(id.toString)

  //val crawler = new EntryPointPathCrawler(SF_GAME_NAME, gameProps, startSeed, gameFold)

  var results:Option[PrecrawledPathNode] = None

  def isThreadRunning():Boolean = return this.isRunning

  override def run(): Unit = {
    this.isRunning = true
    //if (verboseDebugOutput) println("Thread " + Thread.currentThread().getName() + " is running.")

    val out = crawler.crawlGame(maxDepth=maxDepth, pathSoFar = initialPath)
    this.results = out

    //if (verboseDebugOutput) println("Thread " + Thread.currentThread().getName() + " is completed.")
    this.isRunning = false
    this.isCompleted = true
  }

}



class EntryPointPathCrawler(SF_GAME_NAME:String = "coin", gameProps:Map[String, Int], startSeed:Int, gameFold:String) {

  val (success, generator) = GameGenerator.mkGameGenerator(gameName = SF_GAME_NAME, gameProps)
  if (!success) throw new RuntimeException("ERROR creating text game(): " + generator.errorStr)

  val precachedGame = this.mkCachedVariations(numVariationsToMake = 1, startSeed, gameFold)(0)

  // Clear the string LUT
  StepResultHashed.resetLUT()

  /*
   * Game crawling
   */

  // TODO: Return
  def crawlGame(maxDepth:Int = 5, pathSoFar:Array[String] = Array.empty[String]): Option[PrecrawledPathNode] = {
    val result = this.crawlGameHelper(precachedGame._1, pathSoFar, maxDepth)
    return result
  }

  private def crawlGameHelper(precachedGame:TextGame, pathSoFar:Array[String], maxDepth:Int = 5): Option[PrecrawledPathNode] = {
    // Stop case: Check that we haven't crawled too deep
    val curDepth = pathSoFar.length
    if (curDepth >= maxDepth) return None

    // Create a fresh game
    val game = precachedGame.deepCopy()
    //val (_game, _goldPath) = this.generator.mkGameWithGoldPath(seed = 1, "train")
    //val game = _game

    // Step 1: Do actions so far
    var stepResult:StepResult = game.initalStep()
    // Subsequent steps
    for (actionStr <- pathSoFar) {
      stepResult = game.step(actionStr)
    }
    val stepResultHashed = StepResultHashed.mkFromStepResult(stepResult)

    // Check for other stop criteria -- e.g. game winning/losing
    if ((stepResult.taskSuccess) || (stepResult.taskFailure)) return Some( new PrecrawledPathNode(stepResult = stepResultHashed, validSteps = Map[String, PrecrawledPathNode]()) )

    // Step 2: Get possible actions from this step
    val validActions = stepResult.validActions

    val validStepResults = mutable.Map[String, PrecrawledPathNode]()

    // Step 3: For each action, try and crawl it
    var useThreads:Boolean = false
    //if (pathSoFar.length == 0) useThreads = true    // Use threads for the second path step
    if (pathSoFar.length < 2) useThreads = true    // Use threads for the second path step

    if (!useThreads) {
      // Serialized/Non-threaded
      var verboseIdx:Int = 0
      for (validActionStr <- validActions) {
        // Verbose reporting, for vague progress report
        if (pathSoFar.length == 0) {
          println(verboseIdx + " / " + validActions.length)
        }

        val actionsToTake = pathSoFar ++ Array(validActionStr)
        val result = this.crawlGameHelper(precachedGame, actionsToTake, maxDepth)      // Recursive call.  TODO: return
        if (result.isDefined) {
          validStepResults(validActionStr) = result.get
        }

        verboseIdx += 1
      }

    } else {
      // Threaded
      val runners = new Array[CrawlerRunner](validActions.length)

      // Start threads
      for (i <- 0 until validActions.length) {
        val validActionStr = validActions(i)
        val initialPath = pathSoFar ++ Array(validActionStr)
        //runners(i) = new CrawlerRunner(i, SF_GAME_NAME, gameProps, initialPath, startSeed, gameFold, maxDepth)
        runners(i) = new CrawlerRunner(i, this, initialPath, maxDepth)
        runners(i).start()
      }

      // Wait for threads to finish
      var stillRunning = new ArrayBuffer[Int]()     // More elegant ways of doing this
      stillRunning.append(-1)  // Initial

      while (stillRunning.length > 0) {
        stillRunning.clear()

        for (i <- 0 until validActions.length) {
          if (runners(i).isThreadRunning()) {
            stillRunning.append(i)
          } else {
            // Get result
            val validActionStr = validActions(i)
            val result = runners(i).results
            if (result.isDefined) {
              validStepResults(validActionStr) = runners(i).results.get
            }
          }
        }

        Thread.sleep(250)
        println("Threads still running: " + stillRunning.mkString(" "))
      }

    }


    // Pack results into a PrecrawledPathNode
    val node = new PrecrawledPathNode(stepResult = stepResultHashed, validSteps = validStepResults.toMap)
    return Some(node)

  }


  /*
   * Precached games
   */
  def mkCachedVariations(numVariationsToMake:Int = 10, startSeed:Int = 0, gameFold:String = "train"):Array[(TextGame, Array[String])] = {
    val out = new ArrayBuffer[(TextGame, Array[String])]
    println("* Precaching " + numVariationsToMake + " games (" + gameFold + ").")

    for (i <- 0 until numVariationsToMake) {
      val (_game, _goldPath) = this.generator.mkGameWithGoldPath(seed = startSeed+i, gameFold)
      out.append( (_game, _goldPath) )
    }

    out.toArray
  }

  def getPrecachedGame():(TextGame, Array[String]) = {
    return (precachedGame._1.deepCopy(), precachedGame._2)
  }

}



object EntryPointPathCrawler {

  def crawlPath(gameName:String, gameProps:Map[String, Int], variationIdx:Int, gameFold:String, maxDepth:Int, filenameOutPrefix:String) = {
    // Create crawler
    val crawler = new EntryPointPathCrawler(gameName, gameProps.toMap, variationIdx, gameFold)

    println ("Starting crawling...")
    val startTime = System.currentTimeMillis()

    val (game, goldPath) = crawler.getPrecachedGame()
    val precrawledGameTree = crawler.crawlGame(maxDepth)

    val deltaTime = (System.currentTimeMillis() - startTime)
    println("Finished crawling... (time = " + deltaTime + " msec)")
    println("Tree size: " + precrawledGameTree.get.treeSize() + " nodes")
    val (success, winningPath) = precrawledGameTree.get.winningPath()
    println("Shortest winning path (length = " + winningPath.length + "): " + winningPath.mkString(", "))

    println("Gold path: " + goldPath.mkString(", "))

    println("")


    // Run the path
    //val path = precrawledGameTree.get.runPath(winningPath)

    /*
    val path = precrawledGameTree.get.runPath(goldPath)

    for (i <- 0 until path.length) {
      val node = path(i).toStepResult(StepResultHashed.stringLUT.toArray)

      println("Path step " + i)
      println ("> " + goldPath(i) )
      println (node.observationStr)
      println (node.scoreNormalized)

      println("")
    }
     */


    // Convert
    val precrawled = PrecrawledPath.make(root = precrawledGameTree.get, stringLUT = StepResultHashed.stringLUT)
    println( precrawled.StringLUTToString() )

    // Save

    // Create verbose filename
    var propsStr:String = ""
    for (key <- gameProps.keySet.toList.sorted) {
      propsStr += "-" + key + gameProps(key)
    }

    val filenameOut = filenameOutPrefix + "-game" + gameName + "-var" + variationIdx + "-fold" + gameFold + "-maxDepth" + maxDepth + propsStr + ".json"
    println ("Saving..." )
    precrawled.saveToJSON(filenameOut)

  }


  def crawlTWC(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 1                   // Number of locations
    gameProps("numItemsToPutAway") = 1              // Number of items to put away (TWC)
    //gameProps("numDistractorItems") = 0             // Number of distractor items (should be 0 for TWC?)

    val gameName = "twc"
    val maxDepth = 6


    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, filenameOutPrefix = "savetest")
    }

    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i+100, gameFold = "dev", maxDepth, filenameOutPrefix = "savetest")
    }

  }


  def crawlCoin(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 4                   // Number of locations
    gameProps("numDistractorItems") = 1             // Number of distractor items
    gameProps("limitInventorySize") = 0             // Number of distractor items

    val gameName = "coin"
    val maxDepth = 13


    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, filenameOutPrefix = "savetest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i + 100, gameFold = "dev", maxDepth, filenameOutPrefix = "savetest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

  }



  def main(args:Array[String]): Unit = {

    //crawlTWC(numGamesToCrawl = 20)

    crawlCoin(numGamesToCrawl = 20)


  }



}


// Storage class for crawled path
class PrecrawledPathNode(val stepResult:StepResultHashed, val validSteps:Map[String, PrecrawledPathNode]) {
  // A unique ID for this node
  var id:Int = -1

  // Size of the tree starting from this node
  def treeSize():Int = {
    var sum:Int = validSteps.size
    for (validStep <- validSteps.keySet) {
      sum += validSteps(validStep).treeSize()
    }
    return sum
  }

  def hasWinning():Boolean = {
    if (this.stepResult.succ) return true

    for (validStep <- validSteps.keySet) {
      if (validSteps(validStep).hasWinning() == true) return true
    }

    // If we reach here, no winning path found
    return false
  }

  def winningPath():(Boolean, Array[String]) = {
    if (this.stepResult.succ) {
      return (true, Array.empty[String])
    }

    var bestPath:Option[Array[String]] = None
    for (validStepStr <- validSteps.keySet) {

      val (success, path) = validSteps(validStepStr).winningPath()
      if (success) {
        // If we haven't found a winning path yet, OR the new winning path is shorter than the current-best winning path, then replace the current winning path
        if ((bestPath.isEmpty) || (bestPath.get.length > path.length+1)) {
          bestPath = Some(Array(validStepStr) ++ path)
        }
      }
    }

    if (bestPath.isDefined) {
      // Winning path found
      return (true, bestPath.get)
    } else {
      // No winning path found
      return (false, Array.empty[String])
    }

  }


  def runPath(actions:Array[String]):Array[StepResultHashed] = {

    val curAction = actions(0)
    println("")
    println("curAction: " + curAction)
    println("validActions: " + this.validSteps.keySet)
    if (this.validSteps.contains(curAction)) {
      val result = this.validSteps(curAction)
      println("Obs: " + result.stepResult.obs )
      if (actions.length > 1) {
        // Recursive condition
        val remainingActions = actions.slice(1, actions.length)
        println("Remaining actions: " + remainingActions.mkString(", "))
        return Array(result.stepResult) ++ result.runPath(remainingActions)     // Recursive call
      } else {
        // Stop condition
        return Array(result.stepResult)
      }
    } else {
      // Unknown action
      throw new RuntimeException("ERROR: Unknown action (" + curAction + ").")
    }

  }

}


object PrecrawledPathNode {
  var curId:Int = -1

  /*
   * Node ID assignment
   */
  def resetID(): Unit = {
    this.curId = -1
  }

  def getNextId():Int = {
    this.curId += 1
    return this.curId
  }

  // Assign unique IDs to all nodes
  def assignUniqueIDs(in:PrecrawledPathNode): Unit = {
    this.resetID()
    this.assignUniqueIDsHelper(in)
  }

  private def assignUniqueIDsHelper(in:PrecrawledPathNode) {
    // This node
    in.id = this.getNextId()

    // Child nodes
    for (child <- in.validSteps.values) {
      this.assignUniqueIDsHelper(child)
    }
  }

}

