package textworldexpress.goldagent

import textworldexpress.games.TWCGame
import textworldexpress.runtime.PythonInterface
import textworldexpress.symbolicmodule.{ModuleCalc, ModuleKnowledgeBaseTWC}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

class TWCGoldAgent(game:TWCGame) {
  val knownLocations = mutable.Set[String]()
  val placedObjects = mutable.Set[String]()

  /*
   * Movement (random walk)
   */
  private def putAwayObjectsRandomWalk(r:Random, numIterations:Int = 0, lastLocation:String = ""):Boolean = {
    val MAX_ITERATIONS = 25
    val agentLocation = game.agentLocation

    // Stop condition
    if (game.getScore().scoreNormalized >= 1.0f) return true

    // Add agent location
    this.knownLocations.add(agentLocation.name)

    // Check to see if there is anything to pick up here.  If so, pick it all up.
    val visibleObjects = agentLocation.collectVisibleObjects()
    for (obj <- visibleObjects) {
      // If the object is movable, AND we haven't already previously put it down somewhere, then pick it up
      if ((obj.isMovable) && (!this.placedObjects.contains(obj.name))) {
        game.step("take " + obj.name)
      }
    }

    // Check to see if there is anything to put down in its cannonical location
    for (iObj <- game.agentInventory.contents) {
      val cannonicalLocations = iObj.canonicalLocations

      breakable {
        for (vObj <- visibleObjects) {
          if (cannonicalLocations.contains(vObj.name)) {
            // We have likely found the intended container for this object
            // If the container is not open, open it
            if (vObj.isContainer && !vObj.isOpen && vObj.isOpenable) {
              game.step("open " + vObj.name)
            }

            // Put the object into the container
            game.step("put " + iObj.name + " in " + vObj.name)
            placedObjects.add(iObj.name)
            break()
          }
        }
      }

    }


    // Movement
    if (game.locations.length > 1) {
      // If there are multiple locations, then try to move to a new location

      // If all the doors are not open in this location, open them
      if ((agentLocation.doorNorth != null) && (!agentLocation.doorNorth.isOpen)) game.step("open door to north")
      if ((agentLocation.doorSouth != null) && (!agentLocation.doorSouth.isOpen)) game.step("open door to south")
      if ((agentLocation.doorEast != null) && (!agentLocation.doorEast.isOpen)) game.step("open door to east")
      if ((agentLocation.doorWest != null) && (!agentLocation.doorWest.isOpen)) game.step("open door to west")

      // If we reach here, then the location wasn't found directly adjacent to this location.  Pick a random direction and move there.
      var validDirections = new ArrayBuffer[String]
      if (agentLocation.locationNorth != null) validDirections.append("north")
      if (agentLocation.locationSouth != null) validDirections.append("south")
      if (agentLocation.locationEast != null) validDirections.append("east")
      if (agentLocation.locationWest != null) validDirections.append("west")

      if (validDirections.length > 1) {
        // Choose a random direction that isn't where we just came from
        var randIdx = r.nextInt(validDirections.length)
        while (validDirections(randIdx) == lastLocation) {
          randIdx = r.nextInt(validDirections.length)
        }
        game.step("move " + validDirections(randIdx))
      } else {
        game.step("move " + validDirections(0))
      }
    }

    if (numIterations < MAX_ITERATIONS) {
      return this.putAwayObjectsRandomWalk(r, numIterations+1, lastLocation=agentLocation.name)
    } else {
      //throw new RuntimeException("ERROR: Unable to find location (" + location + ")")
      return false
    }
  }




  private def mkGoldPathTWC(r:Random): Boolean = {
    //game.step("look around")
    val stepResult = game.initalStep()
    val success = this.putAwayObjectsRandomWalk(r)
    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathTWC(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}



/*
 * Agent with module support
 */
class TWCGoldAgentWithModule(interface:PythonInterface) {
  val game = interface.game match { case x:TWCGame => x; case _=> null }

  // Make the gold path, using the Calc module
  /*
  private def mkGoldPathTWCWithKBModule(r:Random): Boolean = {
    // Ensure that we're running on the correct game type (ArithmeticGame)
    if (game == null) return false
    // Ensure that the appropriate modules have been enabled
    if (!interface.enabledModuleStrs.contains(ModuleKnowledgeBaseTWC.MODULE_NAME)) {
      println ("TWCGoldAgentWithModule: ERROR: " + ModuleKnowledgeBaseTWC.MODULE_NAME + " is not enabled -- can not generate gold path.")
    }


    // Step 1: read math problem
    interface.step("take math problem")
    interface.step("read math problem")

    // Choose the correct calculator action
    val numArg1 = interface.game.getGenerationProperties()("hidden_num1")
    val numArg2 = interface.game.getGenerationProperties()("hidden_num2")
    val operation = interface.game.getGenerationProperties()("hidden_op")
    var opStr:String = "ERROR: Unknown operation"
    if (operation == 0) {
      opStr = "add " + numArg1 + " " + numArg2
    } else if (operation == 1) {
      opStr = "sub " + numArg1 + " " + numArg2
    } else if (operation == 2) {
      opStr = "mul " + numArg1 + " " + numArg2
    } else if (operation == 3) {
      opStr = "div " + numArg1 + " " + numArg2
    } else {
      println ("Unknown operation (" + operation + ") -- This should never happen.")
      return false
    }
    interface.step(opStr)

    // Step 2: Pick up correct task object
    val correctObjName = game.correctObject
    interface.step("take " + correctObjName.name)

    // Step 3: Put in answer box
    val answerBox = game.answerBox
    interface.step("put " + correctObjName.name + " in " + answerBox.name)

    val success = true

    return success
  }
   */

  val knownLocations = mutable.Set[String]()
  val placedObjects = mutable.Set[String]()

  private def mkGoldPathTWCWithKBModule(r:Random, numIterations:Int = 0, lastLocation:String = ""):Boolean = {
    val MAX_ITERATIONS = 25
    val agentLocation = game.agentLocation

    // Ensure that we're running on the correct game type (TWCGame)
    if (game == null) return false
    // Ensure that the appropriate modules have been enabled
    if (!interface.enabledModuleStrs.contains(ModuleCalc.MODULE_NAME)) {
      println ("TWCGoldAgentWithModule: ERROR: " + ModuleKnowledgeBaseTWC.MODULE_NAME + " is not enabled -- can not generate gold path.")
    }

    // Stop condition
    if (game.getScore().scoreNormalized >= 1.0f) return true

    // Add agent location
    this.knownLocations.add(agentLocation.name)

    // Check to see if there is anything to pick up here.  If so, pick up one thing.
    val visibleObjects = agentLocation.collectVisibleObjects()
    breakable {
      for (obj <- visibleObjects) {
        // If the object is movable, AND we haven't already previously put it down somewhere, then pick it up
        if ((obj.isMovable) && (!this.placedObjects.contains(obj.name))) {
          interface.step("take " + obj.name)
          break()
        }
      }
    }

    // Check to see if there is anything to put down in its cannonical location
    for (iObj <- game.agentInventory.contents) {
      val cannonicalLocations = iObj.canonicalLocations

      interface.step("query " + iObj.name)

      breakable {
        for (vObj <- visibleObjects) {
          if (cannonicalLocations.contains(vObj.name)) {
            // We have likely found the intended container for this object
            // If the container is not open, open it
            if (vObj.isContainer && !vObj.isOpen && vObj.isOpenable) {
              interface.step("open " + vObj.name)
            }

            // Put the object into the container
            interface.step("put " + iObj.name + " in " + vObj.name)
            placedObjects.add(iObj.name)
            break()
          }
        }
      }

    }


    // Movement
    if (game.locations.length > 1) {
      // If there are multiple locations, then try to move to a new location

      // If all the doors are not open in this location, open them
      if ((agentLocation.doorNorth != null) && (!agentLocation.doorNorth.isOpen)) interface.step("open door to north")
      if ((agentLocation.doorSouth != null) && (!agentLocation.doorSouth.isOpen)) interface.step("open door to south")
      if ((agentLocation.doorEast != null) && (!agentLocation.doorEast.isOpen)) interface.step("open door to east")
      if ((agentLocation.doorWest != null) && (!agentLocation.doorWest.isOpen)) interface.step("open door to west")

      // If we reach here, then the location wasn't found directly adjacent to this location.  Pick a random direction and move there.
      var validDirections = new ArrayBuffer[String]
      if (agentLocation.locationNorth != null) validDirections.append("north")
      if (agentLocation.locationSouth != null) validDirections.append("south")
      if (agentLocation.locationEast != null) validDirections.append("east")
      if (agentLocation.locationWest != null) validDirections.append("west")

      if (validDirections.length > 1) {
        // Choose a random direction that isn't where we just came from
        var randIdx = r.nextInt(validDirections.length)
        while (validDirections(randIdx) == lastLocation) {
          randIdx = r.nextInt(validDirections.length)
        }
        interface.step("move " + validDirections(randIdx))
      } else {
        interface.step("move " + validDirections(0))
      }
    }

    if (numIterations < MAX_ITERATIONS) {
      return this.mkGoldPathTWCWithKBModule(r, numIterations+1, lastLocation=agentLocation.name)
    } else {
      //throw new RuntimeException("ERROR: Unable to find location (" + location + ")")
      println ("### timed out")
      return false
    }
  }

  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathTWCWithKBModule(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = interface.history.map(_.userInputStr).toArray.filter(_.length > 0)
    return (true, path)
  }

}
