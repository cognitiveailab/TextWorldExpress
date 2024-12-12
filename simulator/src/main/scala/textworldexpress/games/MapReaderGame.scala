package textworldexpress.games


import textworldexpress.data.{LoadTWCDataJSON, LoadCookingWorldDataJSON}
import textworldexpress.goldagent.{MapReaderGoldAgent}
import textworldexpress.objects.{Box, Coin, DoorMaker, FastObject, Mapbook, Room}
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


// 'taskObjects' just contains the reference to any coin(s) to be collected in the agent's inventory
class MapReaderGameScoring(val taskObjects:ArrayBuffer[FastObject], box:Box) extends Scorer {

  def doScoring(): Unit = {
    // Check status of each object
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false

    // Get a score of 1 for each task object in the inventory
    for (taskObject <- taskObjects) {
      if ((taskObject.currentContainer != null) && (taskObject.currentContainer.name == "inventory")) {
        curScore += 1
      }
    }

    // Get a score of 2 for each task object in the box
    for (taskObject <- taskObjects) {
      if ((taskObject.currentContainer != null) && (taskObject.currentContainer.name == box.name)) {
        curScore += 2
      }
    }

    // Determine if the task is successfully completed or not
    if (curScore == this.calculateMaxScore()) {
      taskSuccess = true
    } else {
      taskSuccess = false
    }

    // Store current scores in 'this.curScore', for the accessor in the base class
    val normalizedScore = curScore/maxScore
    val scores = new GameScore(scoreRaw = curScore, scoreNormalized = normalizedScore, taskSuccess = taskSuccess, taskFailure = taskFailure)
    this.curScore = scores
  }

  // Calculate the maximum possible score for this recipe
  def calculateMaxScore():Double = {
    var maxScore:Double = 0.0
    maxScore = (this.taskObjects.length) * 2
    return maxScore
  }

}



class MapReaderGame(val locations:Array[Room], val taskObjects:ArrayBuffer[FastObject], val mapbook:Mapbook, val box:Box, val startLocation:Room, val endLocation:Room, val actualDistanceApart:Int, val limitInventorySize:Boolean, val seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // Inventory
  var agentInventory = new FastObject("inventory")
  // Add the map to the agent's inventory
  agentInventory.addObject(mapbook)

  // Initial location
  var agentLocation:Room = startLocation

  // Objects that have been deleted from the environment, but whose status should still be tracked
  val deletedObjects = new ArrayBuffer[FastObject]()

  // Scorer
  val scorer:Scorer = new MapReaderGameScoring(taskObjects, box)

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Maximum capacity of the inventory (if limitInventorySize is enabled)
  val inventoryMaxCapacity = this.taskObjects.length + 1

  // Internal game random number generator -- primarily for randomizing valid action list.
  val random = new Random(seed)

  val taskDesc = "Your task is to take the coin that is located in the " + endLocation.name + ", and put it into the box found in the " + startLocation.name + ". A map is provided, that you may find helpful."


  /*
   * Cloning
   */

  // TODO: Still not working (needs location map cloning)
  def deepCopy():MapReaderGame = {

    // Return
    new MapReaderGame(locations, taskObjects, mapbook, box, startLocation, endLocation, actualDistanceApart, limitInventorySize, seed, generationProperties)
  }

  /*
   * Generation Properties
   */

  def getGenerationProperties():Map[String, Int] = return this.generationProperties

  /*
   * Task Description
   */
  def getTaskDescription():String = {
    return this.taskDesc
  }


  /*
   *  Action helpers
   */

  // Get visible objects in the environment from the agent's current location (EXCLUDING inventory)
  def getVisibleObjects():ListBuffer[FastObject] = {
    agentLocation.collectVisibleObjects()
  }

  def getCurrentRoomDescription():String = {
    return this.agentLocation.getDescription()
  }

  /*
   * History
   */
  def getHistory():ArrayBuffer[ActionHistory] = {
    return this.history
  }

  /*
   * Score
   */
  // Returns (score, taskSuccess, taskFailure)
  def getScore():GameScore = this.scorer.getCurrentScore()

  /*
   * Actions
   */

  def actionTake(params:Array[FastObject]):String = this.actionTake(params(0))
  def actionTake(obj:FastObject):String = {
    val numItemsInInventory = this.agentInventory.contents.size
    if ((this.limitInventorySize) && (numItemsInInventory >= inventoryMaxCapacity)) {
      return "Your inventory currently has " + numItemsInInventory + " items, and is full.  You can't pick up another item. "
    }

    // Step 1: remove the object from it's current container
    obj.removeFromCurrentContainer()
    // Step 2: Add to the agent inventory
    agentInventory.addObject(obj)

    return "You take the " + obj.name + "."
  }

  def actionPutIn(params:Array[FastObject]):String = this.actionPutIn(params(0), params(1))
  def actionPutIn(invObject:FastObject, newContainer:FastObject):String = {
    // Step 1: Remove the object from it's current container
    invObject.removeFromCurrentContainer()
    // Step 2: Add to the new container
    newContainer.addObject(invObject)

    return "You put the " + invObject.name + " in the " + newContainer.name + "."
  }

  def actionOpenContainer(params:Array[FastObject]):String = this.actionOpenContainer(params(0))
  def actionOpenContainer(obj:FastObject):String = {
    obj.isOpen = true

    if (obj.contents.isEmpty) {
      return  "You open the " + obj.name + ". It's empty inside."
    }

    val os = new StringBuilder
    os.append("You open the " + obj.name + ". ")
    os.append("The " + obj.name + " contains ")
    os.append(obj.contents.map(_.getDescription()).mkString(", ") + ".")

    return os.toString()

  }

  def actionCloseContainer(params:Array[FastObject]):String = this.actionCloseContainer(params(0))
  def actionCloseContainer(obj:FastObject):String = {
    obj.isOpen = false

    return "You close the " + obj.name + "."
  }

  def actionEat(params:Array[FastObject]):String = this.actionEat(params(0))
  def actionEat(obj:FastObject):String = {
    obj.removeFromCurrentContainer()
    obj.isDeleted = true
    obj.isEaten = true
    deletedObjects.append(obj)

    return "You eat the " + obj.name + ".  It is delicious."
  }

  def actionCook(params:Array[FastObject]):String = this.actionCook(params(0), params(1))
  def actionCook(obj:FastObject, device:FastObject):String = {
    // TODO: Note different kinds of cooking based on stove versus oven.
    obj.isRaw = false
    if (device.name == "stove") {
      obj.isFried = true
      return "You fry the " + obj.name + " with the " + device.name + "."
    } else if (device.name == "oven") {
      obj.isRoasted = true
      return "You roast the " + obj.name + " with the " + device.name + "."
    } else if (device.name == "barbeque") {
      obj.isGrilled = true
      return "You grill the " + obj.name + " with the " + device.name + "."
    }


    return "You cook the " + obj.name + " with the " + device.name + "."
  }

  def actionChop(params:Array[FastObject]):String = this.actionChop(params(0))
  def actionChop(obj:FastObject):String = {
    obj.isChopped = true
    obj.isCut = true
    return "You chop the " + obj.name + "."
  }

  def actionSlice(params:Array[FastObject]):String = this.actionSlice(params(0))
  def actionSlice(obj:FastObject):String = {
    obj.isSliced = true
    obj.isCut = true
    return "You slice the " + obj.name + "."
  }

  def actionDice(params:Array[FastObject]):String = this.actionDice(params(0))
  def actionDice(obj:FastObject):String = {
    obj.isDiced = true
    obj.isCut = true
    return "You dice the " + obj.name + "."
  }

  def actionLookAround(params:Array[FastObject]):String = this.actionLookAround()
  def actionLookAround():String = {
    return this.getCurrentRoomDescription()
  }

  def actionExamine(params:Array[FastObject]):String = this.actionExamine(params(0))
  def actionExamine(obj:FastObject):String = {
    return obj.getDescription()
  }

  def actionMove(params:Array[FastObject]):String = this.actionMove(params(0), params(1))
  def actionMove(door:FastObject, newLoc:FastObject):String = {
    newLoc match {
      case loc:Room => {
        if ((door != null) && (!door.isOpen)) {
          return "You can't move there, the door is closed. "
        }
        this.agentLocation = loc
        return this.agentLocation.getDescription ()
      }
      case _ => throw new RuntimeException("ERROR: Attempting to move to a location that is not a room (" + newLoc.name + ")")
    }
  }

  def actionRead(params:Array[FastObject]):String = this.actionRead(params(0))
  def actionRead(obj:FastObject):String = {
    obj.hasBeenRead = true
    return obj.readText
  }

  def actionInventory(params:Array[FastObject]):String = this.actionInventory()
  def actionInventory():String = {
    val os = new StringBuilder()
    if (this.limitInventorySize) {
      os.append("Inventory (maximum capacity is " + inventoryMaxCapacity + " items): \n")
    } else {
      os.append("Inventory: \n")
    }

    if (this.agentInventory.contents.isEmpty) {
      os.append("  Your inventory is currently empty.\n")
    } else {
      for (obj <- this.agentInventory.contents) {
        os.append("  " + obj.getDescription() + "\n")
      }
    }

    return os.toString
  }


  def actionOpenDoor(params:Array[FastObject]):String = this.actionOpenDoor(params(0), params(1))
  def actionOpenDoor(door:FastObject, locationBeyond:FastObject):String = {
    if (!door.isOpen) {
      door.isOpen = true
      return ("You open the " + door.getDescription() + ", revealing the " + locationBeyond.name + ". ")
    } else {
      return "That is already open. "
    }
  }

  def actionCloseDoor(params:Array[FastObject]):String = this.actionCloseDoor(params(0), params(1))
  def actionCloseDoor(door:FastObject, locationBeyond:FastObject):String = {
    if (door.isOpen) {
      door.isOpen = false
      return ("You close the " + door.getDescription() + " to the " + locationBeyond.name + ". ")
    } else {
      return "That is already closed. "
    }
  }

  def actionTaskDescription(params:Array[FastObject]):String = this.actionTaskDescription()
  def actionTaskDescription():String = {
    return this.taskDesc
  }


  val ACTION_TAKE         = 1
  val ACTION_PUTIN        = 2
  val ACTION_OPEN         = 3
  val ACTION_CLOSE        = 4
  val ACTION_EAT          = 5
  val ACTION_COOK         = 6
  val ACTION_CHOP         = 7
  val ACTION_SLICE        = 8
  val ACTION_DICE         = 9
  val ACTION_LOOKAROUND   = 10
  val ACTION_MOVE         = 11
  val ACTION_READ         = 12
  val ACTION_PREPAREMEAL  = 13
  val ACTION_INVENTORY    = 14
  val ACTION_EXAMINE      = 15
  val ACTION_OPENDOOR     = 16
  val ACTION_CLOSEDOOR    = 17
  val ACTION_TASKDESC     = 18

  val ACTION_INVALID      = 0


  /*
   * Action runner/interpreter
   */
  def runAction(actionIdx:Int, params:Array[FastObject]):String = {

    actionIdx match {
      case ACTION_TAKE => return this.actionTake(params)
      case ACTION_PUTIN => return this.actionPutIn(params)
      //case ACTION_OPEN => return this.actionOpenContainer(params)
      //case ACTION_CLOSE => return this.actionCloseContainer(params)
      //case ACTION_EAT => return this.actionEat(params)
      //case ACTION_COOK => return this.actionCook(params)
      //case ACTION_CHOP => return this.actionChop(params)
      //case ACTION_SLICE => return this.actionSlice(params)
      //case ACTION_DICE => return this.actionDice(params)
      case ACTION_LOOKAROUND => return this.actionLookAround(params)
      case ACTION_MOVE => return this.actionMove(params)
      case ACTION_READ => return this.actionRead(params)
      //case ACTION_PREPAREMEAL => return this.actionPrepareMeal(params)
      case ACTION_INVENTORY => return this.actionInventory(params)
      //case ACTION_EXAMINE => return this.actionExamine(params)
      case ACTION_OPENDOOR => return this.actionOpenDoor(params)
      case ACTION_CLOSEDOOR => return this.actionCloseDoor(params)
      case ACTION_TASKDESC => return this.actionTaskDescription(params)

      case _ => return "That is not a command that I recognize."
    }

  }

  /*
   * Action generation
   */
  def mkActions(visibleObjects:ListBuffer[FastObject]): ListBuffer[ (String, Int, Array[FastObject]) ] = {
    val actionsOut = new ListBuffer[(String, Int, Array[FastObject])]    // (action, callback function, parameters)

    // Generic action
    actionsOut.append( ("look around", ACTION_LOOKAROUND, Array.empty[FastObject]) )
    actionsOut.append( ("inventory", ACTION_INVENTORY, Array.empty[FastObject]) )
    actionsOut.append( ("task", ACTION_TASKDESC, Array.empty[FastObject]))

    // Move actions, based on current location
    if (this.agentLocation.locationNorth != null) actionsOut.append( ("move north", ACTION_MOVE, Array(this.agentLocation.doorNorth, this.agentLocation.locationNorth)) )
    if (this.agentLocation.locationSouth != null) actionsOut.append( ("move south", ACTION_MOVE, Array(this.agentLocation.doorSouth, this.agentLocation.locationSouth)) )
    if (this.agentLocation.locationEast != null) actionsOut.append( ("move east", ACTION_MOVE, Array(this.agentLocation.doorEast, this.agentLocation.locationEast)) )
    if (this.agentLocation.locationWest != null) actionsOut.append( ("move west", ACTION_MOVE, Array(this.agentLocation.doorWest, this.agentLocation.locationWest)) )

    // Door-related actions
    if (this.agentLocation.doorNorth != null) {
      actionsOut.append( ("open door to north", ACTION_OPENDOOR, Array(this.agentLocation.doorNorth, this.agentLocation.locationNorth)) )
      actionsOut.append( ("close door to north", ACTION_CLOSEDOOR, Array(this.agentLocation.doorNorth, this.agentLocation.locationNorth)) )
    }
    if (this.agentLocation.doorSouth != null) {
      actionsOut.append( ("open door to south", ACTION_OPENDOOR, Array(this.agentLocation.doorSouth, this.agentLocation.locationSouth)) )
      actionsOut.append( ("close door to south", ACTION_CLOSEDOOR, Array(this.agentLocation.doorSouth, this.agentLocation.locationSouth)) )
    }
    if (this.agentLocation.doorEast != null) {
      actionsOut.append( ("open door to east", ACTION_OPENDOOR, Array(this.agentLocation.doorEast, this.agentLocation.locationEast)) )
      actionsOut.append( ("close door to east", ACTION_CLOSEDOOR, Array(this.agentLocation.doorEast, this.agentLocation.locationEast)) )
    }
    if (this.agentLocation.doorWest != null) {
      actionsOut.append( ("open door to west", ACTION_OPENDOOR, Array(this.agentLocation.doorWest, this.agentLocation.locationWest)) )
      actionsOut.append( ("close door to west", ACTION_CLOSEDOOR, Array(this.agentLocation.doorWest, this.agentLocation.locationWest)) )
    }

    // Object-centered actions
    for (obj <- visibleObjects) {
      // Check if it can be picked up
      if (obj.isMovable) {
        actionsOut.append(("take " + obj.name, ACTION_TAKE, Array(obj)))
      }
    }

    // Inventory objects
    for (iObj <- this.agentInventory.contents) {

      // For each environment object
      for (eObj <- visibleObjects) {
        if ((eObj.isContainer) && (eObj.isOpen)) {
          actionsOut.append( ("put " + iObj.name + " in " + eObj.name, ACTION_PUTIN, Array(iObj, eObj)) )
        }
      }

      // Readable
      if (iObj.isReadable) {
        actionsOut.append( ("read " + iObj.name, ACTION_READ, Array(iObj)) )
      }

    }


    return actionsOut
  }


  /*
   * Step
   */

  /*
   * Step
   */

  def initalStep():StepResult = {
    return this.step("look around", ACTION_LOOKAROUND, Array.empty[FastObject])
  }

  def step(actionStr:String):StepResult = {
    for (action <- lastValidActions) {
      if (action._1 == actionStr) {
        return this.step(actionStr, action._2, action._3)
      }
    }
    // If we reach here, the action was invalid
    return this.step(actionStr, ACTION_INVALID, Array.empty[FastObject])
  }

  def step(validActionIdx:Int):StepResult = {
    val action = this.lastValidActions(validActionIdx)
    return this.step(action._1, action._2, action._3)
  }

  def step(actionStr:String, actionNumber:Int, actionParams:Array[FastObject]):StepResult = {
    // Run action
    val observationStr = this.runAction(actionNumber, actionParams)
    val wasValidAction = if (actionNumber == ACTION_INVALID) { false } else { true }    // If the action is valid, true, otherwise, false

    // Do scoring
    scorer.doScoring()

    // Get current score
    val curScores = this.getScore()

    // Generate next valid actions
    val visibleObjects = this.getVisibleObjects()
    val validActions = this.mkActions(visibleObjects)
    lastValidActions = validActions
    var validActionStrs = new ArrayBuffer[String](validActions.length)
    for (i <- 0 until validActions.length) validActionStrs.append(validActions(i)._1)
    validActionStrs = random.shuffle(validActionStrs)

    // Generate free-look and inventory strings
    val freeLookStr = this.agentLocation.getDescription()
    val inventoryStr = this.actionInventory()

    // Add to action history
    this.history.append( new ActionHistory(actionStr, observationStr, curScores) )

    // Return
    val result = new StepResult(observationStr=observationStr, freeLookStr=freeLookStr, inventoryStr=inventoryStr, validActions = validActionStrs.toArray, scoreRaw=curScores.scoreRaw, scoreNormalized=curScores.scoreNormalized, taskSuccess=curScores.taskSuccess, taskFailure=curScores.taskFailure, wasValidAction = wasValidAction)
    return result
  }


}


class MapReaderGameGenerator {
  val TWCObjectDatabase = new LoadTWCDataJSON()
  val CookingWorldObjectDatabase = new LoadCookingWorldDataJSON()
  val doorMaker = new DoorMaker()


  def mkEnvironment(r:Random, numLocations:Int, maxDistanceApart:Int, maxDistractorItemsPerLocation:Int, includeDoors:Boolean, fold:String):(ArrayBuffer[Room], ArrayBuffer[FastObject], Mapbook, Box, Room, Room, Int) = {
    val locations = new ArrayBuffer[Room]()

    // Randomly generate locations (blank rooms)
    val locationNames = Set("washroom", "bathroom", "cupboard", "pantry", "basement", "closet", "kitchen", "kitchenette", "launderette", "laundromat", "laundry place", "shower", "sauna", "steam room", "pantry", "attic", "garage", "vault", "cellar", "spare room", "canteen", "cookery", "scullery", "cookhouse", "bedroom", "bedchamber", "chamber", "lounge", "bar", "parlor", "salon", "playroom", "recreation zone", "office", "studio", "workshop", "cubicle", "study", "greenhouse", "outside", "street", "driveway", "supermarket", "alley", "side yard", "living room", "library", "storage room", "backyard")
    val shuffledLocationNames = r.shuffle(locationNames.toList)

    for (i <- 0 until numLocations) {
      val room = new Room(name = shuffledLocationNames(i))

      // Add some number of random distractor objects
      if (maxDistractorItemsPerLocation > 0) {
        val numDistractors = r.nextInt(maxDistractorItemsPerLocation)
        for (j <- 0 until numDistractors) {
          val randObj = TWCObjectDatabase.mkRandomObject(r, fold)
          if (randObj.isDefined) {
            room.addObject(randObj.get)
          }
        }
      }
      locations.append(room)
    }

    // Create connection map
    var map:Option[Array[Array[Room]]] = None

    var attempts:Int = 0
    while ((map.isEmpty) && (attempts < 50)) {
      map = this.mkConnections(r, locations)
      attempts += 1
    }

    if (map.isEmpty) throw new RuntimeException("ERROR: Could not generate connection map")

    // Connect rooms/add doors
    this.connectRoomsFromMap(r, map.get, includeDoors)

    // Check that the map is fully connected
    attempts = 0
    var passes:Boolean = false
    while (!passes && (attempts < 10)) {
      // Note: This is pretty hacky
      val foundRooms = mutable.Set[Room]()
      for (i <- 0 until 10) {
        val roomsAtDistance = getRoomsAtDistance(locations, startLocation = locations(0), distance = i)
        for (room <- roomsAtDistance) foundRooms.add(room)
      }
      //println("Locations: " + locations.map(_.name).mkString(", "))
      //println("Found rooms: " + foundRooms.map(_.name).mkString(", "))

      if (locations.length == foundRooms.size) {
        passes = true
      } else {
        // Try to do more connections, just to the rooms that are inaccessible
        val allRooms = locations.map(_.name).toSet
        val accessibleRooms = foundRooms.map(_.name).toSet
        val inaccessibleRooms = allRooms.diff(accessibleRooms)
        //println ("Trying to reconnect inaccessible rooms (pass " + attempts + ").  Inaccessible rooms: " + inaccessibleRooms.mkString(", "))
        this.connectRoomsFromMap(r, map.get, includeDoors, inaccessibleRooms.toArray)

      }
      attempts += 1
    }

    // Find start/end locations
    val distanceApart = r.nextInt(maxDistanceApart)+1
    val startEndLocations = this.findStartEndLocations(r, locations, distance = distanceApart)
    if (startEndLocations.isEmpty) {
      // TODO: Error message about no locations found that distance apart
      // TODO: Fail gracefully.
      throw new RuntimeException("ERROR: No locations could be found that distance apart (" + maxDistanceApart + "). ")
    }
    val startLocation = startEndLocations.get._1
    val endLocation = startEndLocations.get._2

    // Put the coin in the end location
    val coin = new Coin()
    endLocation.addObject(coin)
    val taskObjects = new ArrayBuffer[FastObject]
    taskObjects.append(coin)

    // Put a box in the start location
    val box = new Box()
    startLocation.addObject(box)

    // Create environment map (to be added to the agent inventory later)
    val mapbook = this.mkMap(r, locations, segmentMode = SEGMENT_MODE_GROUPED)

    return (locations, taskObjects, mapbook, box, startLocation, endLocation, distanceApart)
  }

  // Find two locations that are N units apart
  def findStartEndLocations(r:Random, locations:ArrayBuffer[Room], distance:Int = 1):Option[(Room, Room)] = {
    val MAX_ATTEMPTS = 25
    var attempts:Int = 0

    while (attempts < MAX_ATTEMPTS) {
      // Step 1: Randomly pick a location
      val randomLocationIdx = r.nextInt(locations.length)
      val startLocation = locations(randomLocationIdx)

      // Step 2: Check if there are locations that are 'distance' away from it
      val locationsAtDistance = getRoomsAtDistance(locations, startLocation, distance)
      if (locationsAtDistance.length > 0) {
        val shuffled = r.shuffle(locationsAtDistance)
        val endLocation = shuffled(0)

        return Some((startLocation, endLocation))
      }

      // If we reach here, we failed to find any locations at distance 'distance' from the randomly selected location.  Start the process over again.
      attempts += 1
    }

    // If we reach here, we were not able to find any valid paris, even after MAX_ATTEMPTS.
    return None
  }

  // Helper: Gets all rooms that are distance X away from a given start location.
  private def getRoomsAtDistance(locations:ArrayBuffer[Room], startLocation:Room, distance:Int = 1): ArrayBuffer[Room] = {
    val distances = new Array[Int](locations.length)

    var locationsToCheck = new ArrayBuffer[Room]
    // Step 1: Initialize distances array
    for (i <- 0 until locations.length) {
      if (locations(i).name == startLocation.name) {
        distances(i) = 0
        locationsToCheck.append(startLocation)
      } else {
        distances(i) = -1
      }
    }

    // Step 2: Iterate, determining the distances of each location from the start location
    while (locationsToCheck.length > 0) {
      val nextLocations = new ArrayBuffer[Room]

      for (location <- locationsToCheck) {
        // Get the current distance of this location
        val curDist = distances(locations.indexOf(location))

        // Check neighbours to see if their distance is still unpopulated
        // North
        if (location.locationNorth != null) {
          val idx = locations.indexOf(location.locationNorth)
          if (distances(idx) < 0) {
            distances(idx) = curDist + 1
            nextLocations.append(location.locationNorth)
          }
        }

        // South
        if (location.locationSouth != null) {
          val idx = locations.indexOf(location.locationSouth)
          if (distances(idx) < 0) {
            distances(idx) = curDist + 1
            nextLocations.append(location.locationSouth)
          }
        }

        // East
        if (location.locationEast != null) {
          val idx = locations.indexOf(location.locationEast)
          if (distances(idx) < 0) {
            distances(idx) = curDist + 1
            nextLocations.append(location.locationEast)
          }
        }

        // West
        if (location.locationWest != null) {
          val idx = locations.indexOf(location.locationWest)
          if (distances(idx) < 0) {
            distances(idx) = curDist + 1
            nextLocations.append(location.locationWest)
          }
        }
      }

      locationsToCheck = nextLocations
    }

    // Step 3: Collect a list of the locations that are the appropriate distance away
    val out = new ArrayBuffer[Room]
    for (i <- 0 until locations.length) {
      if (distances(i) == distance) {
        out.append(locations(i))
      }
    }

    // Return
    out
  }




  val SEGMENT_MODE_INDIVIDUAL = 0
  val SEGMENT_MODE_GROUPED    = 1
  def mkMap(r:Random, locations:ArrayBuffer[Room], segmentMode:Int = SEGMENT_MODE_GROUPED):Mapbook = {
    val segments = new ArrayBuffer[String]

    // Step 1: Create an array of the individual map connection segments (e.g. Room X connects to Room Y)
    for (location <- locations) {
      if (segmentMode == SEGMENT_MODE_INDIVIDUAL) {
        if (location.locationNorth != null) segments.append("The " + location.name + " connects to the " + location.locationNorth.name + ".")
        if (location.locationSouth != null) segments.append("The " + location.name + " connects to the " + location.locationSouth.name + ".")
        if (location.locationEast != null) segments.append("The " + location.name + " connects to the " + location.locationEast.name + ".")
        if (location.locationWest != null) segments.append("The " + location.name + " connects to the " + location.locationWest.name + ".")
      } else if (segmentMode == SEGMENT_MODE_GROUPED) {
        val connections = new ArrayBuffer[String]
        if (location.locationNorth != null) connections.append(location.locationNorth.name)
        if (location.locationSouth != null) connections.append(location.locationSouth.name)
        if (location.locationEast != null) connections.append(location.locationEast.name)
        if (location.locationWest != null) connections.append(location.locationWest.name)

        if (connections.length > 0) {
          if (connections.length == 1) {
            segments.append("The " + location.name + " connects to the " + connections(0) + ".")
          } else {
            val shuffledConnections = r.shuffle(connections)
            val connectionsFirst = shuffledConnections.slice(0, shuffledConnections.length-1)
            val connectionsLast = shuffledConnections.last
            segments.append("The " + location.name + " connects to the " + connectionsFirst.mkString(", ") + " and " + connectionsLast + ".")
          }
        }
      } else {
        throw new RuntimeException("ERROR: mkMap(): Unknown segment mode (" + segmentMode + ").")
      }
    }

    // Step 2: Assemble a natural language description of the connections:
    val os = new mutable.StringBuilder()
    os.append("The map reads:\n")
    val segmentsShuffled = r.shuffle(segments)
    for (segmentStr <- segmentsShuffled) {
      os.append("  " + segmentStr + "\n")
    }

    // Step 3: Create map object with map text
    val mapbook = new Mapbook()
    mapbook.readText = os.toString()

    return mapbook
  }

  // Randomly add some number of distractor items to the environment, in their canonical locations, using the TWC database.
  def addTWCItems(r:Random, locations:ArrayBuffer[Room], numToAdd:Int, fold:String): ArrayBuffer[FastObject] = {
    val objectsAdded = new ArrayBuffer[FastObject]
    val objectNamesAdded = new ArrayBuffer[String]

    var attempts:Int = 0
    while ((objectsAdded.length < numToAdd) && (attempts < 100)) {
      // Step 1: Pick a random location
      val randLocIdx:Int = r.nextInt(locations.length)
      val location = locations(randLocIdx)
      val objects = location.contents.toArray.sortBy(_.name)      // TODO: NOTE: EXPENSIVE!!!!  (Currently required for determinism)
      if (objects.length > 0) {
        val randObjIdx = r.nextInt(objects.length)
        val container = objects(randObjIdx)

        //println("location: " + location.name)

        val item = TWCObjectDatabase.mkRandomObjectByLocation(r, container.name, fold)
        //val distractorItem = CookingWorldObjectDatabase.mkRandomObjectByLocation(r, container.name)
        if (item.isDefined) {
          //println ("Item: " + item.get.name)

          if (!objectNamesAdded.contains(item.get.name)) {
            // NOTE: For TWC, instead of adding to the container, we add to the room that the container is in.
            val containerRoom = container.currentContainer

            containerRoom match {
              case x:Room => {
                // Add to room that container is in (rather than the container itself) -- i.e. the object should be "on the floor", needing to be put away.
                x.addObject(item.get)
                // Keep track of the items we add
                objectNamesAdded.append(item.get.name)
                objectsAdded.append(item.get)

                //println ("Added " + item.get.name + " to room " + x.name + " that contains container " + container.name)
              }
              case _ => {
                // do nothing
              }
            }
            //
          }
        } else {
          //println ("distractor not defined")
        }
      }

      attempts += 1
      //println ("Attempts: " + attempts)
    }

    return objectsAdded
  }

  // Connects rooms (adds pointers to other locations in the Room storage classes) based on a given connection map
  def connectRoomsFromMap(r:Random, map:Array[Array[Room]], includeDoors:Boolean, forceRooms:Array[String] = Array.empty[String]): Unit = {
    val CONNECTION_PROBABILITY = 0.50

    for (i <- 0 until map.length) {
      for (j <- 0 until map(i).length) {

        breakable {
          val cell = map(i)(j)
          // Check to see how many possible neighbours this cell has
          var numNeighbours: Int = 0
          if ((i < map.length - 1) && (map(i + 1)(j) != null)) numNeighbours += 1
          if ((i >= 1) && (map(i - 1)(j) != null)) numNeighbours += 1
          if ((j < map(i).length - 1) && (map(i)(j + 1) != null)) numNeighbours += 1
          if ((j >= 1) && (map(i)(j - 1) != null)) numNeighbours += 1


          if (cell != null) {

            // connectRoomsFromMap can be run in two modes.
            // First pass mode: forceRooms is empty -- run on every room.
            // Second pass mode: forceRooms is non-emtpy -- only run on those rooms mentioned.
            if ((!forceRooms.isEmpty) && (!forceRooms.contains(cell.name))) break()

            // Step 1: Check north
            if (i < map.length - 1) {
              val queryLoc = map(i + 1)(j)
              if (queryLoc != null) {
                //if (cell.prefersConnectingTo.contains(queryLoc.name)) {
                if ((numNeighbours == 1) || (r.nextDouble() < CONNECTION_PROBABILITY)) {
                  // Do connection
                  cell.locationNorth = queryLoc
                  queryLoc.locationSouth = cell
                  if (includeDoors) {
                    val door = doorMaker.mkDoor(r, cell.name, queryLoc.name, isOpen = false)
                    if (door.isDefined) {
                      cell.doorNorth = door.get
                      queryLoc.doorSouth = door.get
                    }
                  }
                }
              }
            }

            // Step 2: Check south
            if (i >= 1) {
              val queryLoc = map(i - 1)(j)
              if (queryLoc != null) {
                if ((numNeighbours == 1) || (r.nextDouble() < CONNECTION_PROBABILITY)) {
                  // Do connection
                  cell.locationSouth = queryLoc
                  queryLoc.locationNorth = cell
                  if (includeDoors) {
                    val door = doorMaker.mkDoor(r, cell.name, queryLoc.name, isOpen = false)
                    if (door.isDefined) {
                      cell.doorSouth = door.get
                      queryLoc.doorNorth = door.get
                    }
                  }
                }
              }
            }

            // Step 3: Check east
            if (j >= 1) {
              val queryLoc = map(i)(j - 1)
              if (queryLoc != null) {
                if ((numNeighbours == 1) || (r.nextDouble() < CONNECTION_PROBABILITY)) {
                  // Do connection
                  cell.locationEast = queryLoc
                  queryLoc.locationWest = cell
                  if (includeDoors) {
                    val door = doorMaker.mkDoor(r, cell.name, queryLoc.name, isOpen = false)
                    if (door.isDefined) {
                      cell.doorEast = door.get
                      queryLoc.doorWest = door.get
                    }
                  }
                }
              }
            }

            // Step 4: Check west
            if (j < map(i).length - 1) {
              val queryLoc = map(i)(j + 1)
              if (queryLoc != null) {
                if ((numNeighbours == 1) || (r.nextDouble() < CONNECTION_PROBABILITY)) {
                  // Do connection
                  cell.locationWest = queryLoc
                  queryLoc.locationEast = cell
                  if (includeDoors) {
                    val door = doorMaker.mkDoor(r, cell.name, queryLoc.name, isOpen = false)
                    if (door.isDefined) {
                      cell.doorWest = door.get
                      queryLoc.doorEast = door.get
                    }
                  }
                }
              }
            }
          }
        }

      }
    }

  }

  def displayMap(map:Array[Array[Room]]):String = {
    val os = new mutable.StringBuilder()

    for (i <- 0 until map.size) {
      for (j <- 0 until map(i).size) {
        var cellStr = "--"
        val cell = map(i)(j)
        if (cell != null) cellStr = cell.name

        os.append( cellStr.formatted("%20s") + " ")
      }
      os.append("\n")
    }

    os.toString()
  }


  // Find an empty direction
  def findEmptyDirection(r:Random, map:Array[Array[Room]], locX:Int, locY:Int):(Int, Int) = {
    // Step 1: Randomly shuffle the order of directions to evaluate first
    val orderToCheck = r.shuffle( ArrayBuffer(0, 1, 2, 3) )   // Randomly generate the order to check

    // Step 2: Iterate through the direction order, finding the first available direction
    for (directionRef <- orderToCheck) {
      if (directionRef == 0) {
        // Left
        if ((locX - 1) >= 0) {
          if (map(locX - 1)(locY) == null) return (locX - 1, locY)
        }
      } else if (directionRef == 1) {
        // Right
        if ((locX + 1) < map.length) {
          if (map(locX + 1)(locY) == null) return (locX + 1, locY)
        }
      } else if (directionRef == 2) {
        // Up
        if ((locY - 1) >= 0) {
          if (map(locX)(locY - 1) == null) return (locX, locY - 1)
        }
      } else if (directionRef == 3) {
        // Down
        if ((locY + 1) < map.length) {
          if (map(locX)(locY + 1) == null) return (locX, locY + 1)
        }
      }
    }

    // If no available directions were found, then return (-1, -1)
    return (-1, -1)
  }


  def mkConnections(r:Random, locations:ArrayBuffer[Room]): Option[Array[Array[Room]]] = {
    val GRID_SIZE = 10
    val map = Array.ofDim[Room](GRID_SIZE, GRID_SIZE)

    // Initialize blank map
    for (i <- 0 until GRID_SIZE) {
      for (j <- 0 until GRID_SIZE) {
        map(i)(j) = null
      }
    }

    // Keep track of locations yet to place
    val locationsLeft = new ArrayBuffer[Room]
    locationsLeft.insertAll(0, r.shuffle(locations))     // Randomly shuffle locations in

    // Place the first location in the center
    var lastX = 5
    var lastY = 5
    map(lastX)(lastY) = locationsLeft.last
    locationsLeft.remove(locationsLeft.size-1)      // Remove from the back
    var lastLocation = map(lastX)(lastY)


    var attempts:Int = 0
    val populatedLocations = new ArrayBuffer[(Int, Int)]()
    populatedLocations.append( (lastX, lastY) )

    breakable {
      while (locationsLeft.length > 0) {
        // Pick a random reference point
        val refIdx = r.nextInt(populatedLocations.length)
        lastX = populatedLocations(refIdx)._1
        lastY = populatedLocations(refIdx)._2
        lastLocation = map(lastX)(lastY)


        // Pick a random location
        val locationIdx = r.nextInt(locationsLeft.length)
        val location = locationsLeft(locationIdx)

        //println (attempts + ": Trying to place: " + location.name )

        // NOTE: For MapReader, removed constraint that rooms must prefer to connect to each other
        // Find an empty direction
        val (newX, newY) = findEmptyDirection(r, map, lastX, lastY)
        if (newX != -1) {
          // Set new location
          map(newX)(newY) = location
          // Remove from generation
          locationsLeft.remove(locationIdx)

          // Store that this location has been populated
          populatedLocations.append((newX, newY))

          //println ("\tPlaced at: " + newX + ", " + newY)
        } else {
          //println ("\tCan't find location")
        }

        attempts += 1
        if (attempts > 100) break()
      }
    }

    //println( displayMap(map) )

    if (locationsLeft.length > 0) return None
    return Some(map)
  }


  def mkGame(seed:Long, numLocations:Int = 11, maxDistanceApart:Int = 4, maxDistractorItemsPerLocation:Int = 3, includeDoors:Boolean = false, limitInventorySize:Boolean = false, fold:String = "train"):MapReaderGame = {
    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("numLocations") = numLocations
    props("maxDistanceApart") = maxDistanceApart
    props("maxDistractorItemsPerLocation") = maxDistractorItemsPerLocation
    props("includeDoors") = if (includeDoors) { 1 } else { 0 }
    props("limitInventorySize") = if (limitInventorySize) { 1 } else { 0 }
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1

    // Generate Game
    val r = new Random(seed)
    val (locations, taskObjects, mapbook, box, startLocation, endLocation, actualDistanceApart) = mkEnvironment(r, numLocations, maxDistanceApart, maxDistractorItemsPerLocation, includeDoors, fold)
    props("actualDistanceApart") = actualDistanceApart
    val game = new MapReaderGame( locations.toArray, taskObjects, mapbook, box, startLocation, endLocation, actualDistanceApart, limitInventorySize, generationProperties = props.toMap )

    return game
  }


  def mkGameWithGoldPath(seed:Long, numLocations:Int = 11, maxDistanceApart:Int = 4, maxDistractorItemsPerLocation:Int = 3, includeDoors:Boolean = false, limitInventorySize:Boolean = false, fold:String = "train"):(MapReaderGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 1
    val rg = new Random()

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, numLocations, maxDistanceApart, maxDistractorItemsPerLocation, includeDoors, limitInventorySize, fold)
        val goldAgent = new MapReaderGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, numLocations, maxDistanceApart, maxDistractorItemsPerLocation, includeDoors, limitInventorySize, fold)
    return (game, goldPath)
  }


}
