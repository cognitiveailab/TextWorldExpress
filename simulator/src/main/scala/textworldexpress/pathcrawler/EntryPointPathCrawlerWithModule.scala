package textworldexpress.pathcrawler

import textworldexpress.generator.GameGenerator
import textworldexpress.runtime.PythonInterface
import textworldexpress.struct.{StepResult, TextGame}
import textworldexpress.symbolicmodule.{ModuleCalc, ModuleKnowledgeBaseTWC, ModuleSortByQuantity}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class CrawlerRunnerWithModule(id:Int, crawler:EntryPointPathCrawlerWithModule, initialPath:Array[String], maxDepth:Int=5) extends Thread {
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

class EntryPointPathCrawlerWithModule(SF_GAME_NAME:String = "coin", gameProps:Map[String, Int], startSeed:Int, gameFold:String, enabledModulesStr:String) {

  val (success, generator) = GameGenerator.mkGameGenerator(gameName = SF_GAME_NAME, gameProps)
  if (!success) throw new RuntimeException("ERROR creating text game(): " + generator.errorStr)

  // Make parameter string from parameters (for Interface)
  var params = new ArrayBuffer[String]
  for (key <- gameProps.keySet) {
    params.append(key + "=" + gameProps(key))
  }
  val paramStr = params.mkString(",")


  //val precachedGame = this.mkCachedVariations(numVariationsToMake = 1, startSeed, gameFold)(0)
  /*
  val precachedInterface = new PythonInterface()
  val precachedInitialStepResult = precachedInterface.load(gameName = SF_GAME_NAME, gameFold, seed = startSeed, paramStr = this.paramStr, generateGoldPath = false, enabledModulesStr)
   */


  // Clear the string LUT
  StepResultHashed.resetLUT()



  /*
   * Game crawling
   */

  // TODO: Return
  def crawlGame(maxDepth:Int = 5, pathSoFar:Array[String] = Array.empty[String]): Option[PrecrawledPathNode] = {
    val result = this.crawlGameHelper(pathSoFar, maxDepth)
    return result
  }

  private def crawlGameHelper(pathSoFar:Array[String], maxDepth:Int = 5): Option[PrecrawledPathNode] = {
    // Stop case: Check that we haven't crawled too deep
    val curDepth = pathSoFar.length
    if (curDepth >= maxDepth) return None

    // Create a fresh game
    //val game = precachedGame.deepCopy()
    // Make a game that isn't precached
    //val (_game, _goldPath) = this.generator.mkGameWithGoldPath(seed = startSeed, gameFold)
    //val game = _game

    // New: Make every time (with interface)
    val interface = new PythonInterface()
    var stepResult:StepResult = interface.load(gameName = SF_GAME_NAME, gameFold, seed = startSeed, paramStr = this.paramStr, generateGoldPath = false, enabledModulesStr)

    /*
    // Extra new: Try to deepcopy
    val interface = precachedInterface.deepCopy()
    var stepResult = this.precachedInitialStepResult
     */


    // Step 1: Do actions so far
    // Subsequent steps
    //println("PathSoFar: " + pathSoFar.mkString(", "))
    for (actionStr <- pathSoFar) {
      stepResult = interface.step(actionStr)
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

    // Comment to make unthreaded
    if ((pathSoFar.length < 1) || ((pathSoFar.length < 2) && (validActions.length < 25))) useThreads = true    // Use threads for the second path step

    if (!useThreads) {
      // Serialized/Non-threaded
      var verboseIdx:Int = 0
      for (validActionStr <- validActions) {
        // Verbose reporting, for vague progress report
        if (pathSoFar.length == 0) {
          println(verboseIdx + " / " + validActions.length)
        }

        val actionsToTake = pathSoFar ++ Array(validActionStr)
        val result = this.crawlGameHelper(actionsToTake, maxDepth)      // Recursive call.  TODO: return
        if (result.isDefined) {
          validStepResults(validActionStr) = result.get
        }

        verboseIdx += 1
      }

    } else {
      // Threaded
      val runners = new Array[CrawlerRunnerWithModule](validActions.length)

      // Start threads
      for (i <- 0 until validActions.length) {
        val validActionStr = validActions(i)
        val initialPath = pathSoFar ++ Array(validActionStr)
        //runners(i) = new CrawlerRunner(i, SF_GAME_NAME, gameProps, initialPath, startSeed, gameFold, maxDepth)
        runners(i) = new CrawlerRunnerWithModule(i, this, initialPath, maxDepth)
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
        println("Threads still running (" + stillRunning.length + "): " + stillRunning.mkString(" "))
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

/*
  def getPrecachedGame():(TextGame, Array[String]) = {
    return (precachedGame._1.deepCopy(), precachedGame._2)
  }
*/
  def getGame():(PythonInterface, Array[String]) = {
    val interface = new PythonInterface()
    var stepResult:StepResult = interface.load(gameName = SF_GAME_NAME, gameFold, seed = startSeed, paramStr = this.paramStr, generateGoldPath = false, enabledModulesStr)

    return (interface, interface.goldPath)
  }

}



object EntryPointPathCrawlerWithModule {

  def crawlPath(gameName:String, gameProps:Map[String, Int], variationIdx:Int, gameFold:String, maxDepth:Int, enabledModulesStr:String, filenameOutPrefix:String, humanReadable:Boolean=false) = {
    // Clear the string LUT
    StepResultHashed.resetLUT()

    // Create crawler
    val crawler = new EntryPointPathCrawlerWithModule(gameName, gameProps.toMap, variationIdx, gameFold, enabledModulesStr)

    println ("Starting crawling...")
    val startTime = System.currentTimeMillis()

    val (interface, goldPath) = crawler.getGame()
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
    println ("Converting to precrawled path...")
    val precrawled = PrecrawledPath.make(root = precrawledGameTree.get, stringLUT = StepResultHashed.stringLUT, string2Idx = StepResultHashed.string2IDX.toMap)
    println( precrawled.StringLUTToString() )

    // Save

    // Create verbose filename
    var propsStr:String = ""
    for (key <- gameProps.keySet.toList.sorted) {
      propsStr += "-" + key + gameProps(key)
    }

    val filenameOut = filenameOutPrefix + "-game" + gameName + "-var" + variationIdx + "-fold" + gameFold + "-maxDepth" + maxDepth + propsStr + ".json"
    println ("Saving..." )
    precrawled.saveToJSON(filenameOut, humanReadable)

  }


  def crawlTWC(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 1                   // Number of locations
    gameProps("numItemsToPutAway") = 1              // Number of items to put away (TWC)
    //gameProps("numDistractorItems") = 0             // Number of distractor items (should be 0 for TWC?)

    val gameName = "twc"
    val maxDepth = 6
    val enabledModulesStr = ""

    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest")
    }

    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i+100, gameFold = "dev", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest")
    }

  }


  def crawlCoin(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 4                   // Number of locations
    gameProps("numDistractorItems") = 1             // Number of distractor items
    gameProps("limitInventorySize") = 0             // Number of distractor items

    val gameName = "coin"
    val maxDepth = 5
    val enabledModulesStr = ""

    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i + 100, gameFold = "dev", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

  }


  /*
   * With module
   */
  def crawlArithmeticWithModule(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.

    val gameName = "arithmetic"
    val maxDepth = 6
    val enabledModulesStr = ModuleCalc.MODULE_NAME


    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule-speedtest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

    for (i <- 0 until numGamesToCrawl) {
      try {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i + 100, gameFold = "dev", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule-speedtest")
      } catch {
        case e:Throwable => { println ("ERROR: " + e.toString) }
      }
    }

  }

  def crawlTWCWithModule(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
    gameProps("includeDoors") = 0                   // Disable doors
    gameProps("numLocations") = 1                   // Number of locations
    gameProps("numItemsToPutAway") = 2              // Number of items to put away (TWC)
    //gameProps("numDistractorItems") = 0             // Number of distractor items (should be 0 for TWC?)

    val gameName = "twc"
    val maxDepth = 4
    val enabledModulesStr = ModuleKnowledgeBaseTWC.MODULE_NAME

    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule")
    }
    /*
    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i+100, gameFold = "dev", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule")
    }
     */

  }

  def crawlSortingWithModule(numGamesToCrawl:Int=1): Unit = {
    val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.

    val gameName = "sorting"
    val maxDepth = 7
    val enabledModulesStr = ModuleSortByQuantity.MODULE_NAME

    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule", humanReadable = true)
    }
    /*
    for (i <- 0 until numGamesToCrawl) {
      this.crawlPath(gameName, gameProps.toMap, variationIdx = i+100, gameFold = "dev", maxDepth, enabledModulesStr, filenameOutPrefix = "savetest-withmodule")
    }
     */

  }

  def main(args:Array[String]): Unit = {

    //crawlTWC(numGamesToCrawl = 20)

    //crawlCoin(numGamesToCrawl = 20)
    //crawlCoin(numGamesToCrawl = 1)

    val startTime = System.currentTimeMillis()

    crawlArithmeticWithModule(numGamesToCrawl = 1)

    //crawlTWCWithModule(numGamesToCrawl = 1)

    //crawlSortingWithModule(numGamesToCrawl = 1)

    val deltaTime = System.currentTimeMillis() - startTime
    println ("Runtime: " + (deltaTime / 1000) + " seconds")

  }



}

