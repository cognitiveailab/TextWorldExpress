package textworldexpress.games

import textworldexpress.data.{LoadTWCDataJSON, LoadCookingWorldDataJSON}
import textworldexpress.goldagent.TWCGoldAgent
import textworldexpress.objects.{Backyard, Bathroom, Bedroom, Coin, Corridor, DoorMaker, Driveway, FastObject, Kitchen, LaundryRoom, LivingRoom, Pantry, Room, Street, Supermarket}
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


// 'taskObjects' just contains the reference to any objects in the environment that need to be put away into their cannonical locations
class TWCGameScoring(val taskObjects:ArrayBuffer[FastObject]) extends Scorer {

  def doScoring(): Unit = {
    // Check status of each object
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false

    // The score here is essentially just the number of taskObjects that are in their cannonical locations.
    for (taskObject <- taskObjects) {
      val cannonicalLocations = taskObject.canonicalLocations
      if (taskObject.currentContainer != null) {
        if (taskObject.currentContainer.name == "inventory") {
          // Score of 1 for having the object in the agents inventory
          curScore += 1
        } else if (cannonicalLocations.contains(taskObject.currentContainer.name)) {
          // Score of 2 for placing the object in the correct location
          curScore += 2
        }
      }
    }

    // Determine if the task is successfully completed or not
    if (curScore == (taskObjects.length*2)) {
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



class TWCGame(val locations:Array[Room], val taskObjects:ArrayBuffer[FastObject], limitInventorySize:Boolean, seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // Inventory
  var agentInventory = new FastObject("inventory")

  // Initial location
  var agentLocation:Room = locations(0)

  // Objects that have been deleted from the environment, but whose status should still be tracked
  val deletedObjects = new ArrayBuffer[FastObject]()

  // Scorer
  val scorer:Scorer = new TWCGameScoring(taskObjects)

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Maximum capacity of the inventory (if limitInventorySize is enabled)
  val inventoryMaxCapacity = this.taskObjects.length + 1

  // Internal game random number generator -- primarily for randomizing valid action list.
  val random = new Random(seed)


  /*
   * Cloning
   */

  def deepCopy():TWCGame = {
    val clonedTaskObjects = new ArrayBuffer[FastObject]

    // Step 1: Clone locations
    val locationsClone = new Array[Room](locations.length)
    for (i <- 0 until locations.length) {
      locationsClone(i) = locations(i).deepCopy(existingTaskObjects = taskObjects, copyTaskObjects = clonedTaskObjects)
    }

    // Step 2: Connect rooms
    this.connectClonedMap(locationsClone)

    // Step 3: Create new game
    val game = new TWCGame(locationsClone, clonedTaskObjects, this.limitInventorySize, seed = this.seed, this.generationProperties)

    // Also clone the agent inventory
    game.agentInventory = this.agentInventory.deepCopy(existingTaskObjects = taskObjects, copyTaskObjects = clonedTaskObjects)

    // Return
    game
  }

  // Connect a cloned array of locations in the same way as this map
  private def connectClonedMap(locationsClone:Array[Room]): Unit = {
    for (i <- 0 until locations.length) {
      // Connect locations
      // North
      if (this.locations(i).locationNorth != null) {
        val connectingLocationRef = this.getClonedLocationReference(locationsClone, this.locations(i).locationNorth.name)
        locationsClone(i).locationNorth = connectingLocationRef
        if (this.locations(i).doorNorth != null) {
          val doorClone = this.locations(i).doorNorth.deepCopy()
          locationsClone(i).doorNorth = doorClone
          connectingLocationRef.doorSouth = doorClone
        }
      }

      // South
      if (this.locations(i).locationSouth != null) {
        val connectingLocationRef = this.getClonedLocationReference(locationsClone, this.locations(i).locationSouth.name)
        locationsClone(i).locationSouth = connectingLocationRef
        if (this.locations(i).doorSouth != null) {
          val doorClone = this.locations(i).doorSouth.deepCopy()
          locationsClone(i).doorSouth = doorClone
          connectingLocationRef.doorNorth = doorClone
        }
      }

      // East
      if (this.locations(i).locationEast != null) {
        val connectingLocationRef = this.getClonedLocationReference(locationsClone, this.locations(i).locationEast.name)
        locationsClone(i).locationEast = connectingLocationRef
        if (this.locations(i).doorEast != null) {
          val doorClone = this.locations(i).doorEast.deepCopy()
          locationsClone(i).doorEast = doorClone
          connectingLocationRef.doorWest = doorClone
        }
      }

      // West
      if (this.locations(i).locationWest != null) {
        val connectingLocationRef = this.getClonedLocationReference(locationsClone, this.locations(i).locationWest.name)
        locationsClone(i).locationWest = connectingLocationRef
        if (this.locations(i).doorWest != null) {
          val doorClone = this.locations(i).doorWest.deepCopy()
          locationsClone(i).doorWest = doorClone
          connectingLocationRef.doorEast = doorClone
        }
      }

    }
  }

  // Helper for above
  private def getClonedLocationReference(locationsClone:Array[Room], name:String):Room = {
    for (locationClone <- locationsClone) {
      if (locationClone.name == name) return locationClone
    }
    throw new RuntimeException("getClonedLocationReference(): Location not found (this should never happen).  Location name (" + name + ").")
  }


  /*
   * Generation Properties
   */

  def getGenerationProperties():Map[String, Int] = return this.generationProperties

  /*
   * Task Description
   */
  def getTaskDescription():String = {
    return "Your task is to pick up objects, then place them in their usual locations in the environment."
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

  val ACTION_INVALID      = 0


  /*
   * Action runner/interpreter
   */
  def runAction(actionIdx:Int, params:Array[FastObject]):String = {

    actionIdx match {
      case ACTION_TAKE => return this.actionTake(params)
      case ACTION_PUTIN => return this.actionPutIn(params)
      case ACTION_OPEN => return this.actionOpenContainer(params)
      case ACTION_CLOSE => return this.actionCloseContainer(params)
      //case ACTION_EAT => return this.actionEat(params)
      //case ACTION_COOK => return this.actionCook(params)
      //case ACTION_CHOP => return this.actionChop(params)
      //case ACTION_SLICE => return this.actionSlice(params)
      //case ACTION_DICE => return this.actionDice(params)
      case ACTION_LOOKAROUND => return this.actionLookAround(params)
      case ACTION_MOVE => return this.actionMove(params)
      //case ACTION_READ => return this.actionRead(params)
      //case ACTION_PREPAREMEAL => return this.actionPrepareMeal(params)
      case ACTION_INVENTORY => return this.actionInventory(params)
      //case ACTION_EXAMINE => return this.actionExamine(params)
      case ACTION_OPENDOOR => return this.actionOpenDoor(params)
      case ACTION_CLOSEDOOR => return this.actionCloseDoor(params)

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

      if (obj.isContainer && obj.isOpenable) {
        if (!obj.isOpen) {
          actionsOut.append( ("open " + obj.name, ACTION_OPEN, Array(obj)) )
        } else {
          actionsOut.append( ("close " + obj.name, ACTION_CLOSE, Array(obj)) )
        }
      }
    }


    // Inventory objects
    for (iObj <- this.agentInventory.contents) {

      // For each environment object
      for (eObj <- visibleObjects) {
        if ((eObj.isContainer) && (eObj.isOpen)) {
          actionsOut.append(("put " + iObj.name + " in " + eObj.name, ACTION_PUTIN, Array(iObj, eObj)))
        }
      }

    }

      return actionsOut
  }


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
    // Randomize presentation order of valid actions
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


class TWCGameGenerator {
  val TWCObjectDatabase = new LoadTWCDataJSON()
  //val CookingWorldObjectDatabase = new LoadCookingWorldDataJSON()
  val doorMaker = new DoorMaker()


  def mkEnvironment(r:Random, numLocations:Int, numItemsToPutAway:Int, includeDoors:Boolean, fold:String):(ArrayBuffer[Room], ArrayBuffer[FastObject]) = {
    var locations = new ArrayBuffer[Room]()

    // Add locations
    locations.append( new Kitchen(r, addKnife = false) )
    locations.append( new Pantry(r) )
    locations.append( new Corridor(r) )
    locations.append( new Bedroom(r) )
    //locations.append( new Backyard(r) )     // No TWC objects defined for backyard
    locations.append( new LivingRoom(r) )
    locations.append( new Bathroom(r) )
    locations.append( new LaundryRoom(r) )

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

    // TWC is normally played in environments with only 1, 2, or 3 rooms.  Randomly pick some interconnected rooms, and prune the rest
    var attempts1:Int = 0
    var locationsSubset = ArrayBuffer.empty[Room]
    while ((locationsSubset.isEmpty) && (attempts1 < 50)) {
      val (success, _locationsSubset) = this.pickInterconnectedRooms(r, numLocations, locations)
      locationsSubset = _locationsSubset
    }
    if (locationsSubset.isEmpty) throw new RuntimeException("ERROR: Could not generate connection map subset")

    // Replace locations with new subsampled locations
    locations = locationsSubset
    //println ("subsampledLocations: " + locations.map(_.name).mkString(", "))

    // Add task items
    val taskObjects = this.addTWCItems(r, locations, numToAdd = numItemsToPutAway, fold)

    if (taskObjects.length < numItemsToPutAway) {
      throw new RuntimeException("ERROR: Could not add " + numItemsToPutAway + " items.")
    }

    // Put task objects in random locations
    for (taskObject <- taskObjects) {
      val randomRoom = locations(r.nextInt(locations.length))
      randomRoom.addObject(taskObject)
    }




    return (locations, taskObjects)
  }

  // Prune the location grid to only 'numRoomsDesired' interconnected rooms
  def pickInterconnectedRooms(r:Random, numRoomsDesired:Int, locations:ArrayBuffer[Room]):(Boolean, ArrayBuffer[Room]) = {
    val startRoomIdx:Int = r.nextInt(locations.length)
    var startRoom = locations(startRoomIdx)

    val includedRooms = new ArrayBuffer[Room]()
    includedRooms.append(startRoom)

    var attempts:Int = 0
    while ((includedRooms.length < numRoomsDesired) && (attempts < 10)) {
      // Randomly choose one room
      val randRoom = includedRooms(r.nextInt(includedRooms.length))
      // Now, randomly find a room that's connected to it, if it exists
      val connectedRoom = this.getRandomConnectedRoom(r, randRoom, discludeRooms = includedRooms)
      if (connectedRoom.isDefined) {
        includedRooms.append( connectedRoom.get )
      }
    }

    // If we reach here, and includedRooms is of length 'numRoomsDesired', then subset selection was completed successfully.  If not, exit.
    if (includedRooms.length != numRoomsDesired) {
      return (false, ArrayBuffer.empty[Room])
    }

    // Now that we have the subset of locations, prune any connections that are not those locations.
    val validRoomNames = includedRooms.map(_.name)
    for (room <- includedRooms) {
      // Check each connection, ensuring that it's valid.
      // North
      if ((room.locationNorth != null) && (!validRoomNames.contains(room.locationNorth.name))) {
        room.locationNorth = null
        room.doorNorth = null
      }
      // South
      if ((room.locationSouth != null) && (!validRoomNames.contains(room.locationSouth.name))) {
        room.locationSouth = null
        room.doorSouth = null
      }
      // East
      if ((room.locationEast != null) && (!validRoomNames.contains(room.locationEast.name))) {
        room.locationEast = null
        room.doorEast = null
      }
      // West
      if ((room.locationWest != null) && (!validRoomNames.contains(room.locationWest.name))) {
        room.locationWest = null
        room.doorWest = null
      }
    }

    // Return the new set of locations
    return (true, includedRooms)
  }


  // Randomly pick a room that's connected to the current room
  def getRandomConnectedRoom(r:Random, room:Room, discludeRooms:ArrayBuffer[Room]):Option[Room] = {
    val possibleRooms = new ArrayBuffer[Room]()
    val discludedRoomNames = discludeRooms.map(_.name)

    if ((room.locationNorth != null) && (!discludedRoomNames.contains(room.locationNorth.name))) possibleRooms.append(room.locationNorth)
    if ((room.locationSouth != null) && (!discludedRoomNames.contains(room.locationSouth.name))) possibleRooms.append(room.locationSouth)
    if ((room.locationEast != null) && (!discludedRoomNames.contains(room.locationEast.name))) possibleRooms.append(room.locationEast)
    if ((room.locationWest != null) && (!discludedRoomNames.contains(room.locationWest.name))) possibleRooms.append(room.locationWest)

    if (possibleRooms.length == 0) return None
    val randRoom = possibleRooms( r.nextInt(possibleRooms.length) )
    return Some(randRoom)
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
  def connectRoomsFromMap(r:Random, map:Array[Array[Room]], includeDoors:Boolean): Unit = {
    for (i <- 0 until map.length) {
      for (j <- 0 until map(i).length) {

        val cell = map(i)(j)

        if (cell != null) {
          // Step 1: Check north
          if (i < map.length-1) {
            val queryLoc = map(i+1)(j)
            if (queryLoc != null) {
              if (cell.prefersConnectingTo.contains(queryLoc.name)) {
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
            val queryLoc = map(i-1)(j)
            if (queryLoc != null) {
              if (cell.prefersConnectingTo.contains(queryLoc.name)) {
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
            val queryLoc = map(i)(j-1)
            if (queryLoc != null) {
              if (cell.prefersConnectingTo.contains(queryLoc.name)) {
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
          if (j < map(i).length-1) {
            val queryLoc = map(i)(j+1)
            if (queryLoc != null) {
              if (cell.prefersConnectingTo.contains(queryLoc.name)) {
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
    val GRID_SIZE = 7
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
    var lastX = 3
    var lastY = 3
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

        // If these locations prefer connecting to each other
        if (location.prefersConnectingTo.contains(lastLocation.name)) {
          // Find an empty direction
          val (newX, newY) = findEmptyDirection(r, map, lastX, lastY)
          if (newX != -1) {
            // Set new location
            map(newX)(newY) = location
            // Remove from generation
            locationsLeft.remove(locationIdx)

            // Store that this location has been populated
            populatedLocations.append( (newX, newY) )

            //println ("\tPlaced at: " + newX + ", " + newY)
          } else {
            //println ("\tCan't find location")
          }



        } else {
          //println ("\tDoesn't connect to last")
        }


        attempts += 1
        if (attempts > 100) break()
      }
    }


    //println( displayMap(map) )

    if (locationsLeft.length > 0) return None
    return Some(map)
  }


  //def mkGame(seed:Long, numLocations:Int = 12, numItemsToPutAway:Int = 10, includeDoors:Boolean = true, limitInventorySize:Boolean = true, fold:String = "train"):CoinGame = {
  def mkGame(seed:Long, numLocations:Int = 3, numItemsToPutAway:Int = 4, includeDoors:Boolean = false, limitInventorySize:Boolean = false, fold:String = "train"):TWCGame = {
    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("numLocations") = numLocations
    props("numItemsToPutAway") = numItemsToPutAway
    props("includeDoors") = if (includeDoors) { 1 } else { 0 }
    props("limitInventorySize") = if (limitInventorySize) { 1 } else { 0 }
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1

    // Generate Game

    val r = new Random(seed)
    val (locations, taskObjects) = mkEnvironment(r, numLocations, numItemsToPutAway, includeDoors, fold)
    val game = new TWCGame( locations.toArray, taskObjects, limitInventorySize, generationProperties = props.toMap )

    return game
  }

  def mkGameWithGoldPath(seed:Long, numLocations:Int = 3, numItemsToPutAway:Int = 4, includeDoors:Boolean = true, limitInventorySize:Boolean = true, fold:String = "train"):(TWCGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 50
    val rg = new Random()

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, numLocations, numItemsToPutAway, includeDoors, limitInventorySize, fold)
        val goldAgent = new TWCGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        /*
        if (!success) {
          for (h <- game.history) {
            println ("> " + h.actionStr)
            println ("")
            println (h.observationStr)
            println ("Score: " + h.score)
          }
        }
        */

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, numLocations, numItemsToPutAway, includeDoors, limitInventorySize, fold)
    return (game, goldPath)
  }

}
