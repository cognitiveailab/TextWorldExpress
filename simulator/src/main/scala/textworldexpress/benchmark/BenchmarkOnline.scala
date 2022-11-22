package textworldexpress.benchmark

import textworldexpress.games.CoinGame
import textworldexpress.generator.GameGenerator
import textworldexpress.runtime.PythonInterface
import textworldexpress.struct.TextGame

import scala.io.StdIn.readLine
import scala.util.Random


object BenchmarkOnline {

  def printUsage(): Unit = {
    println ("Usage: BenchmarkOnline <gameName>")
    val twx = new PythonInterface()
    println ("Valid game names: " + twx.getGameNames().toArray.mkString(", "))
  }

  def main(args:Array[String]): Unit = {
    val numEnvs:Int = 100000
    var numStepsPerEnv:Int = 50
    val generateGoldPath:Boolean = false
    val verbose:Boolean = false

    val twx = new PythonInterface()
    var gameName = "coin"

    if (args.length == 0) {
      this.printUsage()
      println ("Missing argument defining game name.  Defaulting to: coin.")

    } else if (args.length == 1) {
      gameName = args(0).toLowerCase.trim()
      if (!twx.getGameNames().toArray().contains(gameName)) {
        println ("Unrecognized game name (" + gameName + ").")
        this.printUsage()
        sys.exit(1)
      }

    } else {
      this.printUsage()
      sys.exit(1)
    }



    val gameFold:String = "train"
    val paramStr:String = ""              // e.g. "numLocations=5, includeDoors=1"
    val properties = Map[String, Int]()   // Use default properties

    println ("Starting Benchmark...")
    val env = twx.loadAndMake(gameName, gameFold, seed = 0, paramStr, generateGoldPath=generateGoldPath)

    var totalSteps:Long = 0
    val startTime = System.currentTimeMillis()

    for (i <- 0 until numEnvs) {
      // Reset environment / Initial step
      var obs = twx.resetWithRandomSeed(gameFold, generateGoldPath=generateGoldPath)

      for (stepNum <- 0 until numStepsPerEnv) {
        if (verbose) {
          print(obs.toString)
        }

        // Pick a random valid action
        val validActions = obs.validActions
        val randIdx = Random.nextInt(validActions.length)
        val validActionStr = validActions(randIdx)

        // Take that action in the environment
        obs = twx.step(validActionStr)
        totalSteps += 1
      }

    }

    val deltaTime = (System.currentTimeMillis() - startTime).toDouble/1000.0

    println ("Environment: " + gameName)
    println ("Tested with: " + numEnvs + " generated environments, with " + numStepsPerEnv + " randomly chosen steps per environment.")

    println ("Delta time: " + deltaTime.formatted("%3.3f") + " sec")
    println ("Rate: " + (deltaTime.toDouble / numEnvs.toDouble) + " sec/env")
    println ("Rate: " + (numEnvs.toDouble / deltaTime.toDouble ) + " envs/sec")
    println ("Rate: " + (totalSteps.toDouble / deltaTime.toDouble ) + " steps/sec (including environment initialization)")


  }


}