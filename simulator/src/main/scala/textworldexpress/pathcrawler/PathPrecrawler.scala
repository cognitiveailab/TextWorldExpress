package textworldexpress.pathcrawler

import textworldexpress.generator.GameGenerator
import textworldexpress.runtime.PythonInterface
import textworldexpress.struct.{StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



//class CrawlerRunner(id:Int, SF_GAME_NAME:String = "coin", gameProps:Map[String, Int], initialPath:Array[String], startSeed:Int=0, gameFold:String="train", maxDepth:Int=5) extends Thread {
class CrawlerRunner1(id:Int, crawler:PathCrawler, initialPath:Array[String], maxDepth:Int=5) extends Thread {
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




class PathCrawler(SF_GAME_NAME:String = "coin", gameProps:Map[String, Int], seed:Int, gameFold:String) {

  val (success, generator) = GameGenerator.mkGameGenerator(gameName = SF_GAME_NAME, gameProps)
  if (!success) throw new RuntimeException("ERROR creating text game(): " + generator.errorStr)

  //val precachedGame = this.mkCachedVariations(numVariationsToMake = 1, startSeed, gameFold)(0)

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
    //val (_game, _goldPath) = this.generator.mkGameWithGoldPath(seed = 1, "train")
    //val game = _game
    val game = this.generator.mkGame(seed = seed, gameFold)
    if (pathSoFar.length == 0) {
      println("Game Generation Propreties: " + game.getGenerationProperties().toString() + ")")
    }

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
        val result = this.crawlGameHelper(actionsToTake, maxDepth)      // Recursive call.  TODO: return
        if (result.isDefined) {
          validStepResults(validActionStr) = result.get
        }

        verboseIdx += 1
      }

    } else {
      // Threaded
      val runners = new Array[CrawlerRunner1](validActions.length)

      // Start threads
      for (i <- 0 until validActions.length) {
        val validActionStr = validActions(i)
        val initialPath = pathSoFar ++ Array(validActionStr)
        //runners(i) = new CrawlerRunner(i, SF_GAME_NAME, gameProps, initialPath, startSeed, gameFold, maxDepth)
        runners(i) = new CrawlerRunner1(i, this, initialPath, maxDepth)
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

}



object PathPrecrawler {

  def crawlPath(gameName:String, gameProps:Map[String, Int], seed:Int, gameFold:String, maxDepth:Int, filenameOutPrefix:String) = {
    // Create crawler
    val crawler = new PathCrawler(gameName, gameProps.toMap, seed, gameFold)

    println ("Starting crawling...")
    val startTime = System.currentTimeMillis()

    //val (game, goldPath) = crawler.getPrecachedGame()
    val precrawledGameTree = crawler.crawlGame(maxDepth)

    val deltaTime = (System.currentTimeMillis() - startTime)
    println("Finished crawling... (time = " + deltaTime + " msec)")
    println("Tree size: " + precrawledGameTree.get.treeSize() + " nodes")
    val (success, winningPath) = precrawledGameTree.get.winningPath()
    println("Shortest winning path (length = " + winningPath.length + "): " + winningPath.mkString(", "))

    //println("Gold path: " + goldPath.mkString(", "))

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
    //## println( precrawled.StringLUTToString() )

    // Save

    // Create verbose filename
    var propsStr:String = ""
    for (key <- gameProps.keySet.toList.sorted) {
      propsStr += "-" + key + gameProps(key)
    }

    val filenameOut = filenameOutPrefix + "-game" + gameName + "-seed" + seed + "-fold" + gameFold + "-maxDepth" + maxDepth + propsStr + ".json"
    println ("Saving..." )
    precrawled.saveToJSON(filenameOut)

  }

  /*
    def crawlTWC(numGamesToCrawl:Int=1): Unit = {
      val gameProps = mutable.Map[String, Int]()      // Game properties. Leave blank for default.
      gameProps("includeDoors") = 0                   // Disable doors
      gameProps("numLocations") = 1                   // Number of locations
      gameProps("numItemsToPutAway") = 1              // Number of items to put away (TWC)
      //gameProps("numDistractorItems") = 0             // Number of distractor items (should be 0 for TWC?)

      val gameName = "twc"
      val maxDepth = 6


      for (i <- 0 until numGamesToCrawl) {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, filenameOutPrefix = "savetest1")
      }

      for (i <- 0 until numGamesToCrawl) {
        this.crawlPath(gameName, gameProps.toMap, variationIdx = i+100, gameFold = "dev", maxDepth, filenameOutPrefix = "savetest1")
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
          this.crawlPath(gameName, gameProps.toMap, variationIdx = i, gameFold = "train", maxDepth, filenameOutPrefix = "savetest1")
        } catch {
          case e:Throwable => { println ("ERROR: " + e.toString) }
        }
      }

      for (i <- 0 until numGamesToCrawl) {
        try {
          this.crawlPath(gameName, gameProps.toMap, variationIdx = i + 100, gameFold = "dev", maxDepth, filenameOutPrefix = "savetest1")
        } catch {
          case e:Throwable => { println ("ERROR: " + e.toString) }
        }
      }

    }
  */


  def printUsage(): Unit = {
    println ("Usage: PathPrecrawler <gameName:Str> <gameFold:Str> <gameSeed:Int> <gameProperties:Str>")
    println ("Where:")
    println ("  gameName is one of: " + GameGenerator.VALID_GAME_NAMES.sorted.mkString(", "))
    println ("  gameFold is one of: train, dev, test")
    println ("  gameSeed is a positive integer (e.g. 1, 2, 3). ")
    println ("  maxDepth is the maximum depth to crawl in the game state tree (maximum of 12 recommended).")
    println ("  gameProperties is an optional comma-delimited list of game properties to set, without spaces (e.g. numLocations=4,includeDoors=1,numDistractorItems=1,limitInventorySize=0)")
    println ("")
    println ("Example:")
    println ("  PathCrawler twc train 0 6 numLocations=1,includeDoors=0,numItemsToPutAway=2")
  }

  def main(args:Array[String]): Unit = {

    // Step 1: Parse command line arguments
    if ((args.length < 4) || (args.length > 5)) {
      println ("ERROR: Expected 3 or 4 arguments (found " + args.length + ").")
      println ("")
      this.printUsage()
      sys.exit(1)
    }

    // Parse game name
    val gameName = args(0).toLowerCase.trim()
    if (!GameGenerator.VALID_GAME_NAMES.contains(gameName)) {
      println ("ERROR: Unknown game name (" + gameName + ").  Valid names: " + GameGenerator.VALID_GAME_NAMES.mkString(", "))
      println ("")
      this.printUsage()
      sys.exit(1)
    }

    // Parse game fold
    val gameFold = args(1).toLowerCase.trim()
    if (!Array("train", "dev", "test").contains(gameFold)) {
      println ("ERROR: Unknown game fold (" + gameFold + ").  Valid folds: train, dev, test")
      println ("")
      this.printUsage()
      sys.exit(1)
    }

    // Parse game seed
    var gameSeed:Int = -1
    try {
      gameSeed = args(2).toInt
    } catch {
      case _:Throwable => {
        println ("ERROR: Can not convert game seed (" + args(2) + ") to integer.")
        println ("")
        this.printUsage()
        sys.exit(1)
      }
    }
    if (gameSeed < 0) {
      println ("ERROR: Game seed (" + gameSeed + ") must be positive.")
      println ("")
      this.printUsage()
      sys.exit(1)
    }

    // TODO: Check for cannonical game seed for the set.

    // Parse game seed
    var maxDepth:Int = -1
    try {
      maxDepth = args(3).toInt
    } catch {
      case _:Throwable => {
        println ("ERROR: Can not convert max depth (" + args(3) + ") to integer.")
        println ("")
        this.printUsage()
        sys.exit(1)
      }
    }
    if (maxDepth < 1) {
      println ("ERROR: maximum crawl depth (" + maxDepth + ") must be positive.")
      println ("")
      this.printUsage()
      sys.exit(1)
    }
    if (maxDepth > 12) {
      println ("WARNING: maximum crawl depth ( " + maxDepth + ") exceeds maximum recommended depth of 12.  This may take some time to crawl.")
      Thread.sleep(2000)
    }


    // Parse game properties string.
    var gamePropsStr = ""
    var gameProps = Map[String, Int]()
    if (args.length == 5) {
      gamePropsStr = args(4)
      val (_props, propErrorStr) = PythonInterface.parseParamStr(gamePropsStr)
      if (propErrorStr.length > 0) {
        println ("ERROR: Error encountered when parsing parameters string (" + gamePropsStr + "):")
        println (propErrorStr)
        println ("")
        this.printUsage()
        sys.exit(1)
      }
      gameProps = _props
    } else {
      println ("No game properties found.   Using default properties.")
    }

    println ("Game Name: " + gameName)
    println ("Game Fold: " + gameFold)
    println ("Game Seed: " + gameSeed)
    println ("Max Depth: " + maxDepth)
    println ("Properties: " + gameProps.toString())


    // Step 2: Do crawling
    this.crawlPath(gameName, gameProps, seed = gameSeed, gameFold, maxDepth, filenameOutPrefix = "precrawledpath")

  }



}

