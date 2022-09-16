package textworldexpress.symbolicmodule

import scala.collection.mutable.ArrayBuffer

class ModuleNavigation(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {
  // The edges (connections between locations) on the map
  var mapEdges = Array.empty[String]    // TODO

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

  }

  // Return a list of locations that connect to Location
  def getConnections(location:String):Array[String] = {

  }

  /*
   * Scraping observations
   */
  override def scrapeObservationStr(observationStr: String): Unit = {
    // TODO
  }

  private def trimStopWords(strIn:String):String = {
    // TODO
    return ""
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
