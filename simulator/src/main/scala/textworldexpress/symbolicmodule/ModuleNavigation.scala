package textworldexpress.symbolicmodule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ModuleNavigation(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {
  // The edges (connections between locations) on the map
  var mapEdges = new ArrayBuffer[MapEdge]()    // TODO

  // The agent's current location
  var currentLocation:String = ""

  // A list of known locations
  var knownLocations = Array.empty[String]    // TODO


  override def getValidCommands(): Array[String] = {
    // Check that there are exactly 2 arguments
    val out = new ArrayBuffer[String]

    // Command 1: Path from X to Y
    /*
    for (locationStr1 <- knownLocations) {
      for (locationStr2 <- knownLocations) {
        if (locationStr1 != locationStr2) {
          out.append("path from " + locationStr1 + " to " + locationStr2)
        }
      }
    }
     */

    // Command 2: Path from current location to Y
    /*
    for (locationStr <- knownLocations) {
      out.append("path to " + locationStr)
    }
     */

    // Command 3: Next step from current location to location X
    for (locationStr <- knownLocations) {
      out.append("next step to " + locationStr)
    }

    // Convert to a set to remove duplicates
    return out.toSet.toArray
  }


  override def runCommand(actionStr: String): String = {
    // Step 1: Make sure this is a valid command
    if (!this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Step 2: Parse the arguments: split the action into the operation and the arguments
    var opStr:String = ""
    var arg1:String = ""
    var arg2:String = ""

    if (actionStr.startsWith("path from")) {
      opStr = "path from"
      val args = actionStr.substring("path from".length).trim().split(" to ")
      arg1 = args(0).trim()
      arg2 = args(1).trim()
    } else if (actionStr.startsWith("path to")) {
      opStr = "path to"
      arg1 = actionStr.substring("path to".length).trim()
    } else if (actionStr.startsWith("next step to")) {
      opStr = "next step to"
      arg1 = actionStr.substring("next step to".length).trim()
    } else {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Step 3: Perform the path operation
    if (opStr == "path from") {
      return this.getPathFromXtoYCommand(arg1, arg2)
    } else if (opStr == "path to") {
      return this.getPathToYCommand(arg1)
    } else if (opStr == "next step to") {
      return this.getNextStepToYCommand(arg1)
    }

    // Default return
    return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
  }


  /*
   * Map Pathfinding
   */

  // Find a path between two locations
  def findPath(startLocation:String, goalLocation:String): Unit = {
    // TODO
  }

  // Add an edge (that two locations connect)
  def addEdge(location1:String, location2:String): Unit = {
    val newEdge1 = new MapEdge(location1.toLowerCase, location2.toLowerCase)

    for (curEdge <- this.mapEdges) {
      if (curEdge.isEqual(newEdge1)) return      // The edge already exists -- do not add
    }

    // If we reach here, the edge is unique -- add it
    this.mapEdges.append(newEdge1)

    // Add a symmetric edge
    val newEdge2 = new MapEdge(location2.toLowerCase, location1.toLowerCase)
    this.mapEdges.append(newEdge2)

    // Update the list of known locations, since we've added a new edge (that might contain a new location)
    this.knownLocations = this.getLocations()
  }

  // Return a list of locations that connect to Location
  def getConnections(location:String):Array[String] = {
    val out = new ArrayBuffer[String]
    for (edge <- this.mapEdges) {
      if (edge.location1 == location) {
        out.append(edge.location2)
      }
    }

    return out.toArray
  }

  // Return a list of all known locations
  def getLocations():Array[String] = {
    val out = mutable.Set[String]()
    for (edge <- this.mapEdges) {
      out.add(edge.location1)
      out.add(edge.location2)
    }

    return out.toArray
  }

  /*
   * Scraping observations
   */

  override def scrapeFreeLookStr(freeLookStr: String): Unit = {
    this.scrapeObservationStr(freeLookStr)
  }

  // Scrape observations for related information
  override def scrapeObservationStr(observationStr: String): Unit = {
    // Case 1: Check for whole maps
    if (observationStr.startsWith("The map reads:")) {
      val lines = observationStr.split("\n")
      for (line <- lines) {
        if (line.contains("connects to the")) {
          val fields = line.split(" connects to the ")
          if (fields.length >= 2) {
            val startLocation = fields(0)
            val endLocations = fields(1).split(",|and")

            val startLocationStr = this.trimStopWords(startLocation)
            for (endLocation <- endLocations) {
              val endLocationStr = this.trimStopWords(endLocation)
              this.addEdge(startLocationStr, endLocationStr)
            }
          }
        }
      }


      // Case 2: In a room
    } else if (observationStr.startsWith("You are in the")) {
      // First, try to determine the current location
      var curLocation = ""
      val sents = observationStr.split(".")
      for (sent <- sents) {
        if (sent.startsWith("You are in the")) {
          print("Sent: " + sent)
          val fields = sent.split("You are in the ")
          curLocation = this.trimStopWords(fields(1))
          this.currentLocation = curLocation      // Store current location globally
          print("CurLocation: " + curLocation)
        }
      }

      // Then, try to determine any connecting locations
      if (curLocation.length > 0) {
        for (sent <- sents) {
          if (sent.trim().startsWith("To the")) {
            val fields = sent.trim().split("To the|you see the|is the")
            if (fields.length == 3) {
              val location = fields.last
              val sanitizedEndLocation = this.trimStopWords(location)
              if (sanitizedEndLocation.length > 0) {
                this.addEdge(curLocation, sanitizedEndLocation)
              }
            }
          }
        }
      }
    }

  }

  private def trimStopWords(strIn:String):String = {
    val stopWords = Array("a", "an", "the", ".")
    val sanitizedStr = strIn.replaceAll("[^\\w\\s]", " ")
    val tokens = sanitizedStr.split(" ")

    val out = new ArrayBuffer[String]
    for (token <- tokens) {
      val sanitizedToken = token.trim()
      if ((sanitizedToken.length > 0) && (!stopWords.contains(sanitizedToken.toLowerCase()))) {
        out.append(token)
      }
    }

    val outStr = out.mkString(" ").trim()
    return outStr
  }


  /*
   * Commands
   */
  def getPathFromXtoYCommand(startLocation:String, endLocation:String):String = {
    // TODO
    return ""
  }

  def getPathToYCommand(endLocation:String):String = {
    // TODO
    return ""
  }

  def getNextStepToYCommand(endLocation:String):String = {
    // TODO
    return ""
  }


}


object ModuleNavigation {
  val MODULE_NAME   = "navigation"
}


class MapEdge(val location1:String, val location2:String) {

  // Check to see if this edge is the same as an input edge
  def isEqual(that:MapEdge): Boolean = {
    if ((this.location1 == that.location1) && (this.location2 == that.location2)) return true
    // Default return
    return false
  }

}