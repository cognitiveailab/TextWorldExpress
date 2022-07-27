package superfast.struct

import programsynthesis.struct.GenSpec
import superfast.tokenizer.Tokenizer

import scala.collection.mutable

/*
 * A counter for storing the strings that the agent encounters from an environment.
 * Helpers for extracting common tokens
 */
class StringCounter {
  val tokenizer = new Tokenizer()
  val counts = mutable.Map[String, Int]()

  /*
   * Adding/getting strings
   */

  // Add an array of strings to the counter
  def add(strsIn:Iterable[String]): Unit = {
    for (strIn <- strsIn) {
      this.add(strIn)
    }
  }

  // Add a string to the counter
  def add(strIn:String): Unit = {
    val santiziedStr = strIn.toLowerCase.trim()

    if (counts.contains(santiziedStr)) {
      counts(santiziedStr) = counts(santiziedStr) + 1
    } else {
      counts(santiziedStr) = 1
    }
  }

  // Get all strings
  def getStrings():Array[String] = {
    return this.counts.keySet.toArray
  }

  /*
   * Addition (from another StringCounter)
   */
  def add(that:StringCounter): Unit = {
    for (tuple <- that.counts) {
      val str = tuple._1
      val count = tuple._2
      if (this.counts.contains(str)) {
        this.counts(str) += count
      } else {
        this.counts(str) = count
      }
    }
  }

  /*
   * Creating observed tokens from strings
   */

  // This one is intended for speed, and directly adds the generated strings to the GenSpec
  def addTokensAndStringsToPool(gs:GenSpec, whichPool:Int, includeBigrams:Boolean = true): Unit = {
    val out = mutable.Set[String]()

    // For each observed string
    for (str <- this.counts.keySet) {
      // Tokenize string
      var lastToken:String = ""

      val tokens = tokenizer.tokenize(str)
      // Store
      for (token <- tokens) {
        val sanitizedToken = token.trim()
        if (sanitizedToken.length > 0) {
          gs.addStaticStringToPool(token, whichPool)

          // Also add bigrams
          if (includeBigrams) {
            val bigram = lastToken + " " + token
            gs.addStaticStringToPool(bigram, whichPool)
            lastToken = token
          }
        }

      }

      // Also store the full string itself
      gs.addStaticStringToPool(str, whichPool)
    }
  }

  // Only adds unigrams to string pool
  def addUnigramsOnlyToPool(gs:GenSpec, whichPool:Int, includeBigrams:Boolean = true): Unit = {
    val out = mutable.Set[String]()

    // For each observed string
    for (str <- this.counts.keySet) {
      // Tokenize string
      var lastToken:String = ""

      val tokens = tokenizer.tokenize(str)
      // Store
      for (token <- tokens) {
        val sanitizedToken = token.trim()
        if (sanitizedToken.length > 0) {
          gs.addStaticStringToPool(token, whichPool)
        }

      }
    }
  }

  def getTokensAndFullStrings():mutable.Set[String] = {
    val out = mutable.Set[String]()

    // For each observed string
    for (str <- this.counts.keySet) {
      // Tokenize string
      val tokens = tokenizer.tokenize(str)
      // Store
      for (token <- tokens) {
        val sanitizedToken = token.trim()
        if (sanitizedToken.length > 0) {
          out.add(token)
        }
      }

      // Also store the full string itself
      out.add(str)
    }

    // Return
    return out
  }

  def getTokenFreqs(): mutable.Map[String, Int] = {
    val tokenCounts = mutable.Map[String, Int]()

    // For each observed string
    for (strTuple <- this.counts.toIterable) {
      val str = strTuple._1       // Observed string
      val count = strTuple._2     // Number of times this string was observed

      // Tokenize string
      val tokens = tokenizer.tokenize(str)

      // Store in counter
      for (token <- tokens) {
        val sanitizedToken = token.trim()
        if (sanitizedToken.length > 0) {
          if (tokenCounts.contains(sanitizedToken)) {
            tokenCounts(sanitizedToken) = tokenCounts(sanitizedToken) + count     // + 1
          } else {
            tokenCounts(sanitizedToken) = count   // 1
          }
        }
      }

    }

    // Return
    return tokenCounts
  }

}


object StringCounter {


  // Quick test of string counter speed
  def main(args:Array[String]): Unit = {

    val numIterations:Int = 100000

    val startTime = System.nanoTime()

    for (i <- 0 until numIterations) {
      val counter = new StringCounter()

      counter.add(Array("go east", "go south", "go north", "go south", "go south", "go south", "take coin"))
      counter.add("This is a test?")
      counter.add(Array("go east", "go south", "go north", "go south", "go south", "go south", "take coin"))
      counter.add("This is a test.")
      counter.add(Array("go east", "go south", "go north", "go south", "go south", "go south", "take coin"))
      counter.add("This is a test!")

      val tokenFreqs = counter.getTokenFreqs()

      /*
      for (key <- tokenFreqs.keySet) {
        println(key + "\t" + tokenFreqs(key))
      }
       */


    }

    val deltaTime = System.nanoTime() - startTime
    val timePerIteration = (deltaTime / numIterations) / 1000

    println ("Time per iteration: " + timePerIteration + " usec")
  }


}