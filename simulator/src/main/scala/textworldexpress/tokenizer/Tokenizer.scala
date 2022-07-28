package textworldexpress.tokenizer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Tokenizer {
  val cache = mutable.HashMap[String, Array[String]]()
  val cacheSentences = mutable.HashMap[String, Array[String]]()


  // Do cheap tokenization
  def tokenize(strIn:String):Array[String] = {
    val delim:String = " "

    // Step 1: Check for cached version
    if (cache.contains(strIn)) {
      return this.cache(strIn)
    }

    // Pass 1: Add space around common punctuation
    val sanitized = strIn.replaceAll("\\p{Punct}", " $0 ")

    // Tokenize
    val tokens = sanitized.split(" ")

    // Store in cache
    this.cache(strIn) = tokens

    // Return
    return tokens
  }


  def mkSentences(strIn:String):Array[String] = {
    // Step 1: Check for cached version
    if (cacheSentences.contains(strIn)) {
      return this.cacheSentences(strIn)
    }

    // Step 2: Perform sentence splitting
    val sentences = strIn.split("[^\\s\\.][^\\.\\n]+")

    // Store in cache
    this.cacheSentences(strIn) = sentences

    // Return
    return sentences
  }


  // Make space-delimited n-grams from an array of tokens
  def mkNgrams(in:Array[String], n:Int=1):Array[String] = {
    val out = new ArrayBuffer[String]()

    for (i <- 0 until in.length - n) {
      val ngram = in.slice(i, i+n).mkString(" ")
      out.append(ngram)
    }

    out.toArray
  }

}
