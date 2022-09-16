package textworldexpress.symbolicmodule

import java.util.StringTokenizer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}
import ModuleSortByQuantity._

class ModuleSortByQuantity(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {
  var observedObjects = Array.empty[ObservedQuantityObject]

  var pregeneratedStrAscending:String = STR_NO_OBSERVED_OBJECTS
  var pregeneratedStrDescending:String = STR_NO_OBSERVED_OBJECTS

  var lastFreeLookStr:String = ""

  override def getValidCommands(): Array[String] = {
    return ModuleSortByQuantity.precachedCommands
  }

  override def runCommand(actionStr: String): String = {
    // Step 1: Make sure this is a valid command
    if (!this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    if (actionStr == CMD_SORT_ASCENDING) {
      return this.pregeneratedStrAscending
    } else if (actionStr == CMD_SORT_DESCENDING) {
      return this.pregeneratedStrDescending
    }

    return "ERROR: ModuleSortByQuantity: Unrecognized command."
  }


  override def scrapeFreeLookStr(freeLookStr: String): Unit = {
    // First, check if the current free look string is the same as the last one -- if so, save time and do not recompute.
    if (lastFreeLookStr == freeLookStr) return
    this.lastFreeLookStr = freeLookStr

    // Tokenize string
    val tokens = ModuleSortByQuantity.tokenizeStr(freeLookStr) ++ Array("", "", "", "")
    val candidates = new ArrayBuffer[ObservedQuantityObject]

    // Search for patterns that look like "5kg of apples" or "5 apples"
    for (i <- 0 until tokens.length-4) {
      // Token 1: number
      if (isNumber(tokens(i))) {
        val quantity = tokens(i)

        // Token 2: Noun or known unit
        if (isWord(tokens(i+1))) {
          var found = false
          // Case 1: Check for something of the form "5kg of apples"
          if (knownUnits.contains(tokens(i+1).toLowerCase)) {
            val unit = tokens(i+1).toLowerCase

            // Token 3: Prep (of)
            if (isPreposition(tokens(i+2))) {

              // Token 4: Noun
              if (isWord(tokens(i+3))) {
                // Candidate for something of the form "5kg of apples"
                val referent = tokens(i) + tokens(i+1) + " " + tokens(i+2) + " " + tokens(i+3)
                val normalizedQuantity = quantity.toDouble * unitMultipliers(unit)
                candidates.append( new ObservedQuantityObject(normalizedQuantity, referent, tokens.slice(i, i+4)) )
                found = true
              }
            }
          }

          // Case 2: Check for something of the form "5 apples"
          if (!found) {
            val referent = tokens(i) + " " + tokens(i+1)
            candidates.append( new ObservedQuantityObject(quantity.toDouble, referent, tokens.slice(i, i+1) ))
          }
        }
      }
    }

    // Store
    this.observedObjects = candidates.toArray

    // Make sure there were objects found
    if (this.observedObjects.length == 0) {
      this.pregeneratedStrAscending = STR_NO_OBSERVED_OBJECTS
      this.pregeneratedStrDescending = STR_NO_OBSERVED_OBJECTS
    }

    // Pre-generate strings
    val sorted = this.observedObjects.sortBy(_.quantity)

    val osAscending = new StringBuilder()
    osAscending.append("The observed items, sorted in order of increasing quantity, are: ")
    osAscending.append( sorted.map(_.referent).mkString(", ") + ".")
    this.pregeneratedStrAscending = osAscending.toString()

    val osDescending = new StringBuilder()
    osDescending.append("The observed items, sorted in order of decreasing quantity, are: ")
    osDescending.append( sorted.reverse.map(_.referent).mkString(", ") + ".")
    this.pregeneratedStrDescending = osDescending.toString()

  }

}

// Storage class
class ObservedQuantityObject(val quantity:Double, val referent:String, val tokens:Array[String]) {

}

object ModuleSortByQuantity {
  val STR_NO_OBSERVED_OBJECTS = "No objects with quantities have been observed."

  val CMD_SORT_ASCENDING = "sort ascending"
  val CMD_SORT_DESCENDING = "sort descending"
  val precachedCommands = Array(CMD_SORT_ASCENDING, CMD_SORT_DESCENDING)

  val knownUnits = Array("mm", "cm", "m", "mg", "g", "kg", "ml", "l")
  val unitMultipliers = Map("mm" -> 1, "cm" -> 10, "m" -> 1000, "mg" -> 1, "g" -> 1000, "kg" -> 1000000, "ml" -> 1, "l" -> 1000)



  def tokenizeStr(strIn:String): Array[String] = {
    //return strIn.split("[\\p{Punct}\\s]+")
    val tokenizer = new StringTokenizer(strIn, ",?.! \n\r", true)
    val tokens = new ArrayBuffer[String]()
    while (tokenizer.hasMoreTokens) {
      val token = tokenizer.nextToken().trim()
      if (token.length > 0) {

        breakable {
          // Check for special case: Break apart numbers and known affixes
          val tokenLower = token.toLowerCase
          for (knownUnit <- knownUnits) {
            if (tokenLower.endsWith(knownUnit)) {
              val onlyPrefix = tokenLower.substring(0, tokenLower.length - knownUnit.length)
              if (isNumber(onlyPrefix)) {
                // We've found something of the form "5kg" or "10ml" -- split
                tokens.append(onlyPrefix)
                tokens.append(tokenLower.substring(tokenLower.length - knownUnit.length))
                break()
              }
            }
          }

          // Normal case: add tokens
          tokens.append(token)
        }
      }
    }
    return tokens.toArray
  }

  // Check if a string is a number
  def isNumber(strIn:String): Boolean = {
    val num = strIn.toDoubleOption
    if (num.isDefined) return true
    // Otherwise
    return false
  }

  // Check if a string is a preposition
  def isPreposition(strIn:String):Boolean = {
    val preps = Array("of", "in", "on", "over", "under")
    val sanitizedStr = strIn.trim().toLowerCase
    if (preps.contains(sanitizedStr)) return true
    // Otherwise
    return false
  }

  // Treats anything that has one or more letters as a word
  def isWord(strIn:String):Boolean = {
    for (i <- 0 until strIn.length) {
      if (strIn.charAt(i).isLetter) return true
    }
    // Otherwise
    return false
  }


  def main(args:Array[String]): Unit = {
    val strIn = "You are in the supermarket. In one part of the room you see a box, that is empty. There is also a showcase that has 4 watermelons, 30 limes, 44 coconuts, and 8 brocollis on it."
    //val strIn = "You are in the supermarket. In one part of the room you see a box, that is empty. There is also a showcase that has 4kg of watermelons, 30kg of limes, 44g of coconuts, and 8mg of brocollis on it."

    //val tokens = tokenizeStr(strIn)
    //println( tokens.mkString(", "))

    val module = new ModuleSortByQuantity(Map[String, Int]())
    module.scrapeFreeLookStr(strIn)
    println (module.pregeneratedStrAscending)
    println (module.pregeneratedStrDescending)

  }

}