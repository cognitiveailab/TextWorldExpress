package textworldexpress.goldagent

import textworldexpress.games.CookingWorldGame

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

class CookingWorldGoldAgent(game:CookingWorldGame) {
  val foundIngredients = new ArrayBuffer[String]()
  val mapConnections = mutable.Map[String, mutable.Set[String]]()

  /*
   * Map creation
   */
  private def makeMapCurrentLocation(): Unit = {
    val agentLocation = game.agentLocation
    if (((agentLocation.doorNorth == null) || (agentLocation.doorNorth.isOpen)) && (agentLocation.locationNorth != null))  this.addMapConnection(agentLocation.name, agentLocation.locationNorth.name)
    if (((agentLocation.doorSouth == null) || (agentLocation.doorSouth.isOpen)) && (agentLocation.locationSouth != null))  this.addMapConnection(agentLocation.name, agentLocation.locationSouth.name)
    if (((agentLocation.doorEast == null) || (agentLocation.doorEast.isOpen)) && (agentLocation.locationEast != null))  this.addMapConnection(agentLocation.name, agentLocation.locationEast.name)
    if (((agentLocation.doorWest == null) || (agentLocation.doorWest.isOpen)) && (agentLocation.locationWest != null))  this.addMapConnection(agentLocation.name, agentLocation.locationWest.name)
  }

  private def addMapConnection(location1:String, location2:String): Unit = {
    if (!mapConnections.contains(location1)) this.mapConnections(location1) = mutable.Set[String]()
    if (!mapConnections.contains(location2)) this.mapConnections(location2) = mutable.Set[String]()

    mapConnections(location1).add(location2)
    mapConnections(location2).add(location1)
  }

  /*
   * Map navigation
   */
  private def findPathToLocation(curLocation:String, goalLocation:String, pathSoFar:Array[String] = Array.empty[String], numIterations:Int = 0): Array[String] = {
    val MAX_ITERATIONS = 8
    val out = new ArrayBuffer[Array[String]]

    // Stop condition
    if (curLocation == goalLocation) return pathSoFar
    if (numIterations > MAX_ITERATIONS) return Array.empty[String]

    // Recurse condition
    if (this.mapConnections.contains(curLocation)) {
      var out = Array.empty[String]
      val connectingLocations = this.mapConnections(curLocation)
      for (connectingLocation <- connectingLocations) {
        val path = this.findPathToLocation(connectingLocation, goalLocation, pathSoFar ++ Array(connectingLocation), numIterations+1)
        if (path.length > 0) {
          if ((out.length > 0) && (path.length < out.length)) {
            // Store this new, shorter path
            out = path
          }
        }
      }
      return out
    }

    return Array.empty[String]
  }


  /*
   * Movement (random walk)
   */
  private def moveToLocationRandomWalk(r:Random, location:String, numIterations:Int = 0, lastLocation:String = ""):Boolean = {
    val MAX_ITERATIONS = 35
    val agentLocation = game.agentLocation

    // Stop condition
    if (agentLocation.name == location) return true

    // Create map
    this.makeMapCurrentLocation()

    // Go to a currently visible location
    // North
    if ((agentLocation.locationNorth != null) && (agentLocation.locationNorth.name == location) && (agentLocation.doorNorth == null || agentLocation.doorNorth.isOpen)) {
      game.step("move north")
      this.makeMapCurrentLocation()
      return true
    }
    // South
    if ((agentLocation.locationSouth != null) && (agentLocation.locationSouth.name == location) && (agentLocation.doorSouth == null || agentLocation.doorSouth.isOpen)) {
      game.step("move south")
      this.makeMapCurrentLocation()
      return true
    }
    // East
    if ((agentLocation.locationEast != null) && (agentLocation.locationEast.name == location) && (agentLocation.doorEast == null || agentLocation.doorEast.isOpen)) {
      game.step("move east")
      this.makeMapCurrentLocation()
      return true
    }
    // West
    if ((agentLocation.locationWest != null) && (agentLocation.locationWest.name == location) && (agentLocation.doorWest == null || agentLocation.doorWest.isOpen)) {
      game.step("move west")
      this.makeMapCurrentLocation()
      return true
    }


    // If all the doors are not open in this location, open them
    // North
    if ((agentLocation.doorNorth != null) && (!agentLocation.doorNorth.isOpen)) game.step("open door to north")
    if ((agentLocation.locationNorth != null) && (agentLocation.locationNorth.name == location)) {
      this.makeMapCurrentLocation()
      game.step("move north")
      this.makeMapCurrentLocation()
      return true
    }
    // South
    if ((agentLocation.doorSouth != null) && (!agentLocation.doorSouth.isOpen)) game.step("open door to south")
    if ((agentLocation.locationSouth != null) && (agentLocation.locationSouth.name == location)) {
      this.makeMapCurrentLocation()
      game.step("move south")
      this.makeMapCurrentLocation()
      return true
    }
    // East
    if ((agentLocation.doorEast != null) && (!agentLocation.doorEast.isOpen)) game.step("open door to east")
    if ((agentLocation.locationEast != null) && (agentLocation.locationEast.name == location)) {
      this.makeMapCurrentLocation()
      game.step("move east")
      this.makeMapCurrentLocation()
      return true
    }
    // West
    if ((agentLocation.doorWest != null) && (!agentLocation.doorWest.isOpen)) game.step("open door to west")
    if ((agentLocation.locationWest != null) && (agentLocation.locationWest.name == location)) {
      this.makeMapCurrentLocation()
      game.step("move west")
      this.makeMapCurrentLocation()
      return true
    }

    // If we reach here, then the location wasn't found directly adjacent to this location.  Pick a random direction and move there.
    var validDirections = new ArrayBuffer[String]
    if (agentLocation.locationNorth != null)  validDirections.append("north")
    if (agentLocation.locationSouth != null)  validDirections.append("south")
    if (agentLocation.locationEast != null)   validDirections.append("east")
    if (agentLocation.locationWest != null)   validDirections.append("west")

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

    if (numIterations < MAX_ITERATIONS) {
      return this.moveToLocationRandomWalk(r, location, numIterations+1, lastLocation=agentLocation.name)
    } else {
      //throw new RuntimeException("ERROR: Unable to find location (" + location + ")")
      return false
    }
  }


  // Top-level control for moving to a location
  private def moveToLocation(r:Random, location:String): Boolean = {
    // First, see if we are already there
    if (game.agentLocation.name == location) return true

    // Second, see if there is a known path to get to that location
    val path = this.findPathToLocation(game.agentLocation.name, location)
    if (path.length > 0) {
      for (pathLoc <- path) {
        if ((game.agentLocation.locationNorth != null) && (game.agentLocation.locationNorth.name == pathLoc)) {
          game.step("move north")
        } else if ((game.agentLocation.locationSouth != null) && (game.agentLocation.locationSouth.name == pathLoc)) {
          game.step("move south")
        } else if ((game.agentLocation.locationEast != null) && (game.agentLocation.locationEast.name == pathLoc)) {
          game.step("move east")
        } else if ((game.agentLocation.locationWest != null) && (game.agentLocation.locationWest.name == pathLoc)) {
          game.step("move west")
        }
      }
      return true
    }

    // Last, if we don't know how to get there, then just attempt a random walk
    return this.moveToLocationRandomWalk(r, location)
  }


  /*
   * Collecting ingredients
   */

  private def collectIngredientsHere(): Unit = {
    var containersToOpen = Array("fridge", "kitchen cupboard")

    // Get a list of all visible objects
    var visibleObjectsInitial = game.agentLocation.collectVisibleObjects()

    // Check if there are any recipe ingredients here
    for (ingredient <- game.recipe) {
      for (obj <- visibleObjectsInitial) {
        if (obj.name == ingredient.name) {
          game.step("take " + obj.name)
          this.foundIngredients.append(obj.name)
        }
      }
    }

    // If we've found all the ingredients, no need to open any containers
    if (this.foundIngredients.length == game.recipe.length) return

    // Check if there are any containers that should be opened
    for (obj <- visibleObjectsInitial) {
      if (containersToOpen.contains(obj.name)) {
        game.step("open " + obj.name)

        val visibleObjects = game.agentLocation.collectVisibleObjects()

        // Check if there are any recipe ingredients here
        for (ingredient <- game.recipe) {
          for (obj <- visibleObjects) {
            if (obj.name == ingredient.name) {
              game.step("take " + obj.name)
              this.foundIngredients.append(obj.name)
            }
          }
        }

        // Close any opened containers
        game.step("close " + obj.name)

        // If we've found all the ingredients, stop opening containers
        if (this.foundIngredients.length == game.recipe.length) return
      }
    }

    /*
    // NOTE: Uncomment below for a more-generic look-in-every-container solution
    var openedContainers = new ArrayBuffer[String]

    // Mid-point: If we've found all the ingredients, move on
    if (this.foundIngredients.length == game.recipe.length) return

    // Otherwise, check all other openable containers
    openedContainers.clear()
    for (obj <- visibleObjects) {
      if ((obj.isOpenable) && (!obj.isOpen) && (!containersToOpen.contains(obj.name))) {
        game.step("open " + obj.name)
        openedContainers.append(obj.name)
      }
    }

    visibleObjects = game.agentLocation.collectVisibleObjects()

    // Check if there are any recipe ingredients here
    for (ingredient <- game.recipe) {
      for (obj <- visibleObjects) {
        if (obj.name == ingredient.name) {
          game.step("take " + obj.name)
          this.foundIngredients.append(obj.name)
        }
      }
    }

    // Close any opened containers
    for (cObjName <- openedContainers) {
      game.step("close " + cObjName)
    }
     */

  }


  private def mkGoldPathKitchen(r:Random): Boolean = {
    val stepResult = game.initalStep()

    // Step 1: Move to kitchen
    if (!this.moveToLocation(r, location="kitchen")) return false

    // Step 2: Take cookbook
    game.step("take cookbook")

    // Step 3: Read cookbook
    val out1 = game.step("read cookbook")
    //println (out1._1)

    // Step 4: Take knife
    // Check to see if knife is visible
    val visibleObjects = game.agentLocation.collectVisibleObjects()
    if (visibleObjects.filter(_.name == "knife").size > 0) {
      // Knife is on a visible surface
      game.step("take knife")
    } else {
      // Knife is not visible -- try opening cutlery drawer
      game.step("open cutlery drawer")
      val visibleObjects2 = game.agentLocation.collectVisibleObjects()
      if (visibleObjects2.filter(_.name == "knife").size == 0) {
        // Knife still not visible -- try looking in kitchen cupboard
        game.step("open kitchen cupboard")
      }
      game.step("take knife")
    }


    // Step 5: Start collecting ingredients
    val locationsToVisit = Array("kitchen", "pantry", "backyard", "supermarket")
    breakable {
      for (location <- locationsToVisit) {
        if (!this.moveToLocation(r, location)) return false
        this.collectIngredientsHere()
        if (this.foundIngredients.length == game.recipe.length) break()
      }
    }
    // Move back to kitchen
    if (!this.moveToLocation(r, "kitchen")) return false

    /*
    println ("required ingredients: " + game.recipe.map(_.name).mkString(", "))
    println ("inventory: " + game.agentInventory.contents.mkString(", "))

    if (this.foundIngredients.length != game.recipe.length) {
      println ("ERROR: Did not find all ingredients!")
    }
    */


    // Step 6: Start preparing ingredients
    // Step 6A: Chopping
    for (ingredient <- game.recipe) {
      if (ingredient.preparation.contains("chopped")) game.step("chop " + ingredient.name)
      if (ingredient.preparation.contains("sliced")) game.step("slice " + ingredient.name)
      if (ingredient.preparation.contains("diced")) game.step("dice " + ingredient.name)
    }

    // Step 6B: Stove and Oven
    for (ingredient <- game.recipe) {
      if (ingredient.preparation.contains("fried")) game.step("cook " + ingredient.name + " in stove")
      if (ingredient.preparation.contains("roasted")) game.step("cook " + ingredient.name + " in oven")
    }

    // Step 6C: Barbeque or Toaster
    for (ingredient <- game.recipe) {
      if (ingredient.preparation.contains("grilled")) {
        if (game.locations.length < 3) {
          if (!this.moveToLocation(r, location="kitchen")) return false
          game.step("cook " + ingredient.name + " in toaster")
        }
        else {
          if (!this.moveToLocation(r, location="backyard")) return false
          game.step("cook " + ingredient.name + " in barbeque")
        }
      }
    }
    if (!this.moveToLocation(r, "kitchen")) return false


    // Step 7: Prepare meal
    game.step("prepare meal")

    // Step 8: Eat meal
    val out = game.step("eat meal")

    // Should be finished
    /*
    println(out._1)
    if (out._6 != 1.0) {
      println ("ERROR: Score not 1.0")
    }
    */

    return true
  }


  def mkGoldPath(r:Random):(Boolean, Array[String]) = {
    val success = this.mkGoldPathKitchen(r)
    if (!success) return (false, Array.empty[String])

    // Success
    val path = game.history.map(_.actionStr).toArray
    return (true, path)
  }

}
