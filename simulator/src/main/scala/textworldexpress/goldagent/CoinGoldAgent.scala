package textworldexpress.goldagent

import textworldexpress.games.CoinGame

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


class CoinGoldAgent(game:CoinGame) {
  val knownLocations = mutable.Set[String]()

  /*
   * Movement (random walk)
   */
  private def findCoinRandomWalk(r:Random, numIterations:Int = 0, lastLocation:String = ""):Boolean = {
    val MAX_ITERATIONS = 25
    val agentLocation = game.agentLocation

    // Add agent location
    this.knownLocations.add(agentLocation.name)

    // Stop condition: Check to see if the coin is here.
    val visibleObjects = agentLocation.collectVisibleObjects()
    for (obj <- visibleObjects) {
      if (obj.name == "coin") {
        game.step("take coin")
        return true
      }
    }

    // If there are other locations to move to, try to move
    if (game.locations.length > 1) {
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
      return this.findCoinRandomWalk(r, numIterations+1, lastLocation=agentLocation.name)
    } else {
      //throw new RuntimeException("ERROR: Unable to find location (" + location + ")")
      return false
    }
  }




  private def mkGoldPathCoin(r:Random): Boolean = {
    //game.step("look around")
    val stepResult = game.initalStep()
    val success = this.findCoinRandomWalk(r)
    return success
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathCoin(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}
