package textworldexpress.pathcrawler

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// Convert a (winning) path from a series of steps, into a series of 1-step games.
class PathStepifyer {

  // Find all winning paths, up to some maximum length (lengthVariation) beyond the minimum length path.
  // e.g. lengthVariation=2 would return all winning paths at minLength, minLength+1, and minLength+2.
  def findWinningPaths(gameIn:PrecrawledPath, lengthVariation:Int=0): Array[Array[GoldPathStep]] = {
    // Step 1: Get winning paths
    val winningPaths = gameIn.findWinningPaths()
    if (winningPaths.length == 0) throw new RuntimeException("ERROR: No winning paths found.")


    // Trim all beyond some length
    val minPathLength = winningPaths.map(_.length).min
    val maxPathLength = minPathLength + lengthVariation

    val out = new ArrayBuffer[ Array[GoldPathStep] ]
    for (path <- winningPaths) {
      if (path.length <= maxPathLength) {
        out.append(path)
      }
    }

    // Return
    return out.toArray
  }

  // Convert a game and a path into a series of smaller 1-step games.
  // chunkSize allows the games to be into larger pieces (e.g. 2 step games, 3 step games, etc).
  def stepifyPath(gameIn:PrecrawledPath, path:Array[GoldPathStep], chunkSize:Int=1): Array[PrecrawledPath] = {
    val simplifiedPaths = new ArrayBuffer[PrecrawledPath]

    // For each step in the path
    for (i <- 0 until path.length-chunkSize) {
      val nodeLUT = new ArrayBuffer[PrecrawledNode]

      // Step 1: the path nodes
      for (chunkIdx <- 0 until chunkSize) {
        val curStep = path(i + chunkIdx)
        val nextStep = path(i + chunkIdx + 1)


        // Step 1A (Nodes 'chunkSize'): The start node (the current step)
        // Find winning node index
        val actionStr = nextStep.actionStr // Actual action string
        //println("actionStr: " + actionStr)
        val actionStrIdx = gameIn.stringLUT.indexOf(actionStr) // Index of action string in hashmap
        //println("actionStrIdx: " + actionStrIdx)
        //println(curStep.node.steps)
        val winningStepNodeHashIdx = curStep.node.steps(actionStrIdx) // The idx of the node that should be taken on the next step    //Might not need this
        //println("winningStepNodeHashIdx: " + winningStepNodeHashIdx)

        //val currentStepNode = curStep.node.convertToSingleStep(winningStepHashIdx = winningStepNodeHashIdx, idxOfWinningNode = 1, idxOfFailingNode = 2)
        val currentStepNode = curStep.node.convertToSingleStep(winningStepHashIdx = actionStrIdx, idxOfWinningNode = chunkIdx+1, idxOfFailingNode = chunkSize+1)
        nodeLUT.append(currentStepNode)
      }

      // Step 2 (Node 'chunkSize'+1): The winning node (the next step on the winning path)
      val winningResult = StepResultHashed.mkBlankWithScore(score = 1.0, scoreNormalized = 1.0, succ = true, fail = false)
      val winningNode = new PrecrawledNode(result = winningResult, steps = Map[Int, Int]())     // Winning score, blank next-action map
      nodeLUT.append(winningNode)

      // Step 3 (Node 'chunkSize'+2): The failure node (any step that isn't the winning/next step on the winning path)
      val failingResult = StepResultHashed.mkBlankWithScore(score = 0.0, scoreNormalized = 0.0, succ = false, fail = true)
      val failingNode = new PrecrawledNode(result = failingResult, steps = Map[Int, Int]())     // Winning score, blank next-action map
      nodeLUT.append(failingNode)

      // Step 4: Create precrawled path
      val out = new PrecrawledPath(nodeLUT.toArray, gameIn.stringLUT)
      val outReduced = this.minimizeStringLUT(out, excludeNodes = Array(nodeLUT.size-1, nodeLUT.size-2))   // Exclude last two nodes (the faux winning/failing nodes)
      simplifiedPaths.append(outReduced)

      //println(outReduced.StringLUTToString())
      //println(outReduced.exportToJSONStr())

    }

    return simplifiedPaths.toArray
  }


  // Reducing the number of nodes in a precrawled game means the number of strings used in the game will likely be less.
  // This function sets any unused strings to blank, reducing the number of lexical possibilities that a genetic algorithm will have to explore.
  def minimizeStringLUT(gameIn:PrecrawledPath, excludeNodes:Array[Int] = Array.empty[Int]):PrecrawledPath = {
    // Step 1: Find a list of the strings used in this game
    val stringsUsedIdx = mutable.Set[Int]()

    for (i <- 0 until gameIn.nodeLUT.length) {
      if (!excludeNodes.contains(i)) {
        val node = gameIn.nodeLUT(i)

        stringsUsedIdx.add(node.result.obs)
        stringsUsedIdx.add(node.result.inv)
        stringsUsedIdx.add(node.result.look)

        for (actIdx <- node.result.acts) {
          stringsUsedIdx.add(actIdx)
        }
      }
    }

    // Step 2: Create new LUT, with unused strings blanked out
    val out = new Array[String](gameIn.stringLUT.length)
    for (i <- 0 until out.length) {
      if (stringsUsedIdx.contains(i)) {
        out(i) = gameIn.stringLUT(i)
      } else {
        // Blank -- this string is unused in this game
        out(i) = ""
      }
    }

    // Step 3: Create new precrawled game with the new, reduced string LUT.
    return new PrecrawledPath(nodeLUT = gameIn.nodeLUT, stringLUT = out)
  }


}



object PathStepifyer {

  def main(args:Array[String]): Unit = {

    // Step 1: Load precrawled paths
    val precrawledPaths = new ArrayBuffer[PrecrawledPath]
    val numPerFold = 1

    for (i <- 0 until numPerFold) {
      val filename = "savetest-gametwc-var" + i + "-foldtrain-maxDepth6-includeDoors0-numItemsToPutAway1-numLocations1.json"
      val precrawledPath = PrecrawledPath.loadFromJSON(filename).get
      precrawledPaths.append(precrawledPath)
    }

    println ("")
    // Step 2: Find gold path(s)
    val stepifyer = new PathStepifyer()
    for (i <- 0 until precrawledPaths.length) {
      val precrawledPath = precrawledPaths(i)
      val goldPaths = stepifyer.findWinningPaths( precrawledPath, lengthVariation = 0 )     // Only the shortest path

      // Convert to single-step games
      for (j <- 0 until goldPaths.length) {
        println ("")
        println ("Gold path " + j)
        val goldPath = goldPaths(j)
        println (goldPath.map(_.actionStr).mkString(", "))

        val chunkSize = 2
        stepifyer.stepifyPath(precrawledPath, goldPath, chunkSize)

      }

    }

  }

}
