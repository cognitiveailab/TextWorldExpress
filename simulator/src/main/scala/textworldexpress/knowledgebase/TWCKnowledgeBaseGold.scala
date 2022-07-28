package textworldexpress.knowledgebase

import textworldexpress.data.LoadTWCDataJSON
import textworldexpress.tokenizer.Tokenizer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._


/*
 * A gold object -> location knowledge graph for the TWC data
 */
class TWCKnowledgeBaseGold(database:LoadTWCDataJSON = new LoadTWCDataJSON()) extends KnowledgeBase {
  val rows = this.mkObjectLocationTable()
  val numColumns = rows(0).length

  val precachedQueries = this.mkPrecachedLUT()


  /*
   *  Accessors
   */
  // TODO: Should probably be tokenized
  def queryKB(queryStr:String):KBResult = {
    // If the queryStr isn't in the precache, then it likely wasn't found -- return blank.
    if (!precachedQueries.contains(queryStr)) {
      return new KBResult(Array.empty[Array[String]])
    }

    // Otherwise, fetch the results from the cache, copy them (so there's a fresh iterator for whatever will use them), and return them.
    return precachedQueries(queryStr).copy()
  }

  // An uncached version of the above, which is generally used to build the cache.
  def queryKBNoCache(queryStr:String):KBResult = {
    val out = new ArrayBuffer[Array[String]]
    val queryStrSanitized = queryStr.toLowerCase

    // For each row
    for (i <- 0 until rows.length) {
      val row = rows(i)

      // Check each cell to see if it contains the query term
      breakable {
        for (j <- 0 until row.length) {
          val cell = row(j)
          if (cell.contains(queryStr)) {
            // If it does, add it to the search results
            out.append(row)
            break()
          }
        }
      }

    }

    return new KBResult(out.toArray)
  }

  // TODO: Should probably be tokenized
  def queryKBColumn(queryStr:String, colIdx:Int):KBResult = {
    // Bound checking: Make sure the requested column index exists
    if (colIdx >= this.numColumns) return new KBResult(Array.empty[Array[String]])

    // Perform the query
    val out = new ArrayBuffer[Array[String]]
    val queryStrSanitized = queryStr.toLowerCase

    // For each row
    for (i <- 0 until rows.length) {
      val row = rows(i)

      // Check cell to see if it contains the query term
      val cell = row(colIdx)
      if (cell.contains(queryStr)) {
        // If it does, add this row to the search results
        out.append(row)
        break()
      }

    }

    return new KBResult(out.toArray)
  }

  /*
   * Loading
   */
  private def mkObjectLocationTable():Array[Array[String]] = {
    // Assemble a list of all objects, and shuffle it
    var allObjs = (database.allObjsTrain ++ database.allObjsDev ++ database.allObjsTest).toList
    val r = new Random(seed = 0)
    allObjs = r.shuffle(allObjs)


    // Create a table of obj -> locatedIn -> location tuples.
    val out = new ArrayBuffer[Array[String]]
    for (obj <- allObjs) {
      for (location <- obj.locations) {
        val row = Array(obj.name.toLowerCase, "located", location.toLowerCase)
        out.append(row)
      }
    }

    // Return
    out.toArray
  }

  /*
   * Precaching
   */

  def mkPrecachedLUT():Map[String, KBResult] = {
    val tokenizer = new Tokenizer()

    println (" * Precaching queries for knowledge base... ")

    // Step 1: Find all strings used in the table
    val allStrings = mutable.Set[String]()

    for (row <- rows) {
      for (cell <- row) {
        allStrings.add(cell.toLowerCase)
      }
    }

    // Step 2: Tokenize all strings, add all n-grams
    for (str <- allStrings) {
      val tokens = tokenizer.tokenize(str)
      for (n <- 1 to 4) {
        val ngrams = tokenizer.mkNgrams(tokens, n)

        for (ngram <- ngrams) {
          allStrings.add(ngram)
        }
      }
    }

    // Step 3: Perform all queries for all n-grams, precache the results
    val out = mutable.Map[String, KBResult]()
    for (queryStr <- allStrings) {
      val result = this.queryKBNoCache(queryStr)
      out(queryStr) = result
    }

    println (" * Precached " + out.size + " queries.")

    /*
    //DEBUG: Print queries
    for (key <- allStrings) {
      println (key + ":\t")
      val results = out(key).copy()
      while (results.hasNext()) {
        println ("\t" + results.getNext().mkString(","))
      }
    }
     */

    return out.toMap
  }

  /*
   * String methods
   */

  override def toString():String = {
    val os = new StringBuilder

    for (i <- 0 until rows.length) {
      os.append(i + ": " + rows(i).mkString("\t") + "\n")
    }

    return os.toString
  }

}


