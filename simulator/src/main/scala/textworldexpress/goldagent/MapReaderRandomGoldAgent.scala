package textworldexpress.goldagent

import textworldexpress.games.{MapReaderGame, MapReaderRandomGame}
import textworldexpress.objects.Room
import textworldexpress.runtime.PythonInterface
import textworldexpress.symbolicmodule.{ModuleCalc, ModuleNavigation}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class MapReaderRandomGoldAgent(game:MapReaderRandomGame) {
  val knownLocations = mutable.Set[String]()

  /*
   * Movement (random walk)
   */
  private def findCoinUsingMap(r:Random, numIterations:Int = 0, lastLocation:String = ""):Boolean = {
    val agentLocation = game.agentLocation

    // Step 1: Read task description
    game.step("task")

    // Step 2: Read map
    game.step("read map")

    // Step 3: Start walking to location
    if (!this.navigateToLocation(game.locations, game.startLocation, game.endLocation)) {
      //return false
    }

    // Step 4: Take coin
    game.step("take coin")

    // Step 5: Return to start location
    if (!this.navigateToLocation(game.locations, game.endLocation, game.startLocation)) {
      //return false
    }

    // Step 6: Put coin in box
    game.step("put coin in box")

    return true
  }


  private def mkGoldPathCoin(r:Random): Boolean = {
    //game.step("look around")
    val stepResult = game.initalStep()
    val success = this.findCoinUsingMap(r)
    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathCoin(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

  /*
   * Map pathfinding
   */

  private def navigateToLocation(locations:Array[Room], startLocation:Room, endLocation:Room): Boolean = {
    val locationSteps = this.getLocationPath(game.locations, startLocation, endLocation).reverse
    //println ("startLocation: " + startLocation.name)
    //println ("endLocation: " + endLocation.name)
    //println ("Location steps: " + locationSteps.mkString(", "))

    for (locationStep <- locationSteps) {

      // Find which direction the next step is in, and take it

      // North
      if ((game.agentLocation.locationNorth != null) && (game.agentLocation.locationNorth.name == locationStep)) {
        // Check to see if there is a door that needs to be opened first
        if ((game.agentLocation.doorNorth != null) && (!game.agentLocation.doorNorth.isOpen)) {
          game.step("open door to north")
        }
        // Move north
        game.step("move north")

        // South
      } else if ((game.agentLocation.locationSouth != null) && (game.agentLocation.locationSouth.name == locationStep)) {
        // Check to see if there is a door that needs to be opened first
        if ((game.agentLocation.doorSouth != null) && (!game.agentLocation.doorSouth.isOpen)) {
          game.step("open door to south")
        }
        game.step("move south")

        // East
      } else if ((game.agentLocation.locationEast != null) && (game.agentLocation.locationEast.name == locationStep)) {
        // Check to see if there is a door that needs to be opened first
        if ((game.agentLocation.doorEast != null) && (!game.agentLocation.doorEast.isOpen)) {
          game.step("open door to east")
        }
        game.step("move east")

        // West
      } else if ((game.agentLocation.locationWest != null) && (game.agentLocation.locationWest.name == locationStep)) {
        // Check to see if there is a door that needs to be opened first
        if ((game.agentLocation.doorWest != null) && (!game.agentLocation.doorWest.isOpen)) {
          game.step("open door to west")
        }
        game.step("move west")
      } else {
        if (game.agentLocation.name == locationStep) {
          // Already at this location -- likely just the first or last step from the pathfinding algorithm.  Just ignore this one.
        } else {
          // If we reach here, the path must be incorrect, as the next location on the path isn't accessible from the current location.
          println("ERROR: Unable to find location (" + locationStep + ")")
          println("FROM Location: " + game.agentLocation.getDescription())
          println("")
          return false
        }
      }

    }

    // If we reach here, the navigation was successful
    return true
  }

  // Helper: Gets the distance of all rooms from a given start room
  private def getLocationPath(locations:Array[Room], startLocation:Room, endLocation:Room): Array[String] = {
    val alreadyExplored = mutable.Set[String]()

    //println ("* getLocationPath: Started...")

    val root = new TreeNode(startLocation)
    var locationsToCheck = new ArrayBuffer[TreeNode]
    locationsToCheck.append(root)

    // Step 2: Iterate, determining the distances of each location from the start location
    while (locationsToCheck.length > 0) {
      val nextLocations = new ArrayBuffer[TreeNode]

      for (node <- locationsToCheck) {
        val location = node.location

        // Check for stop condition
        if (location.name == endLocation.name) {
          //println ("* getLocationPath: Found path...")
          //println ("(" + node.getPathToRoot().mkString(", ") + ")")
          return node.getPathToRoot()
        }

        // Check neighbours to see if their distance is still unpopulated
        // North
        if (location.locationNorth != null) {
          if (!alreadyExplored.contains(location.locationNorth.name)) {
            val newNode = new TreeNode(location.locationNorth, parent = Some(node))
            node.addChild(newNode)
            nextLocations.append(newNode)
          }
        }

        // South
        if (location.locationSouth != null) {
          if (!alreadyExplored.contains(location.locationSouth.name)) {
            val newNode = new TreeNode(location.locationSouth, parent = Some(node))
            node.addChild(newNode)
            nextLocations.append(newNode)
          }
        }

        // East
        if (location.locationEast != null) {
          if (!alreadyExplored.contains(location.locationEast.name)) {
            val newNode = new TreeNode(location.locationEast, parent = Some(node))
            node.addChild(newNode)
            nextLocations.append(newNode)
          }
        }

        // West
        if (location.locationWest != null) {
          if (!alreadyExplored.contains(location.locationWest.name)) {
            val newNode = new TreeNode(location.locationWest, parent = Some(node))
            node.addChild(newNode)
            nextLocations.append(newNode)
          }
        }
      }

      locationsToCheck = nextLocations
    }

    // If we reach here, no path was found
    //println ("* getLocationPath: ERROR: Did not find path...")
    return Array.empty[String]
  }


}



/*
 * Agent with module support (MapReaderRandom)
 */
class MapReaderRandomGoldAgentWithModule(interface:PythonInterface) {
  val game = interface.game match { case x:MapReaderRandomGame => x; case _=> null }

  // Make the gold path, using the Calc module
  private def mkGoldPathMapReaderWithNavModule(r:Random): Boolean = {
    // Ensure that we're running on the correct game type (MapReaderRandomGame)
    if (game == null) return false
    // Ensure that the appropriate modules have been enabled
    if (!interface.enabledModuleStrs.contains(ModuleNavigation.MODULE_NAME)) {
      println ("MapReaderRandomGoldAgentWithModule: ERROR: " + ModuleNavigation.MODULE_NAME + " is not enabled -- can not generate gold path.")
    }

    // Step 1: Read task description
    interface.step("task")

    // Step 2: Read map
    interface.step("read map")

    // Step 3: Start walking to location
    if (!this.navigateToLocationUsingModule(game.endLocation)) {
      //return false
    }

    // Step 4: Take coin
    interface.step("take coin")

    // Step 5: Return to start location
    if (!this.navigateToLocationUsingModule(game.startLocation)) {
      //return false
    }

    // Step 6: Put coin in box
    interface.step("put coin in box")

    return true

  }

  private def navigateToLocationUsingModule(endLocation:Room): Boolean = {
    val MAX_ITER = 20
    var numIter:Int = 0

    while (game.agentLocation.name != endLocation.name) {
      val stepResult = interface.step("next step to " + endLocation.name)
      val obs = stepResult.observationStr

      val fields = obs.split(":")
      if (fields.length == 2) {
        val locationStep = fields(1).trim()

        // North
        if ((game.agentLocation.locationNorth != null) && (game.agentLocation.locationNorth.name.toLowerCase == locationStep)) {
          // Check to see if there is a door that needs to be opened first
          if ((game.agentLocation.doorNorth != null) && (!game.agentLocation.doorNorth.isOpen)) {
            interface.step("open door to north")
          }
          // Move north
          interface.step("move north")

          // South
        } else if ((game.agentLocation.locationSouth != null) && (game.agentLocation.locationSouth.name.toLowerCase == locationStep)) {
          // Check to see if there is a door that needs to be opened first
          if ((game.agentLocation.doorSouth != null) && (!game.agentLocation.doorSouth.isOpen)) {
            interface.step("open door to south")
          }
          interface.step("move south")

          // East
        } else if ((game.agentLocation.locationEast != null) && (game.agentLocation.locationEast.name.toLowerCase == locationStep)) {
          // Check to see if there is a door that needs to be opened first
          if ((game.agentLocation.doorEast != null) && (!game.agentLocation.doorEast.isOpen)) {
            interface.step("open door to east")
          }
          interface.step("move east")

          // West
        } else if ((game.agentLocation.locationWest != null) && (game.agentLocation.locationWest.name.toLowerCase == locationStep)) {
          // Check to see if there is a door that needs to be opened first
          if ((game.agentLocation.doorWest != null) && (!game.agentLocation.doorWest.isOpen)) {
            interface.step("open door to west")
          }
          interface.step("move west")
        } else {
          if (game.agentLocation.name.toLowerCase == locationStep) {
            // Already at this location -- likely just the first or last step from the pathfinding algorithm.  Just ignore this one.
          } else {
            // If we reach here, the path must be incorrect, as the next location on the path isn't accessible from the current location.
            println("ERROR: Unable to find location (" + locationStep + ")")
            println("FROM Location: " + game.agentLocation.getDescription())
            println("")
            return false
          }
        }
      }

      numIter += 1
      if (numIter > MAX_ITER) {
        println ("ERROR: Timed out on navigating path (MAX_ITER = " + MAX_ITER + ")")
        return false
      }

    }

    // If we reach here, the navigation was successful
    return true
  }

  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathMapReaderWithNavModule(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = interface.history.map(_.userInputStr).toArray.filter(_.length > 0)
    return (true, path)
  }

}
