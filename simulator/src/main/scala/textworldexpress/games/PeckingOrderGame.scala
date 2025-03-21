package textworldexpress.games

import textworldexpress.goldagent.PeckingOrderGoldAgent
import textworldexpress.objects.{Backyard, Bathroom, Bedroom, Box, BundleOfObjects, Coin, Corridor, DoorMaker, Driveway, FastObject, Instructions, Kitchen, LaundryRoom, LivingRoom, MathProblem, Pantry, Room, Street, Supermarket}
import textworldexpress.preprocessing.ArithmeticProblem
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}



class PeckingOrderScoring(val objectOrder:Array[FastObject], val inventory:FastObject) extends Scorer {

  def doScoring(): Unit = {
    // Check status of each object
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false


    // Check how many items are in the inventory
    val numItems = inventory.contents.size
    // If the number of items in the inventory is greater than the number of task items, then there is too much in the inventory -- failure
    if (numItems > objectOrder.length) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // Check that the first 'numItems' items are in the inventory
    for (i <- 0 until numItems) {
      if (objectOrder(i).currentContainer.name == "inventory") {
        // In the correct spot
        curScore += 1
      } else {
        // Task failure
        val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
        this.curScore = scores
        return
      }
    }

    // If we failed the task, then set the score to -1, regardless of progress
    if (taskFailure) {
      val scores = new GameScore(scoreRaw = -1, scoreNormalized = -1, taskSuccess = false, taskFailure = true)
      this.curScore = scores
      return
    }

    // If we're at the maximum score, set the task success to be true
    if (curScore >= this.calculateMaxScore()) {
      taskSuccess = true
    }

    // Store current scores in 'this.curScore', for the accessor in the base class
    val normalizedScore = curScore/maxScore
    val scores = new GameScore(scoreRaw = curScore, scoreNormalized = normalizedScore, taskSuccess = taskSuccess, taskFailure = taskFailure)
    this.curScore = scores
  }

  // Calculate the maximum possible score for this recipe
  def calculateMaxScore():Double = {
    var maxScore:Double = this.objectOrder.size.toDouble
    return maxScore
  }

}



class PeckingOrderGame(val locations:Array[Room], val objectOrder:Array[FastObject], val instructionBook:Instructions, val seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // Inventory
  var agentInventory = new FastObject("inventory")

  // Initial location
  var agentLocation:Room = locations(0)

  // Objects that have been deleted from the environment, but whose status should still be tracked
  val deletedObjects = new ArrayBuffer[FastObject]()

  // Scorer
  val scorer:Scorer = new PeckingOrderScoring(objectOrder, this.agentInventory)

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Maximum capacity of the inventory (if limitInventorySize is enabled)
  val inventoryMaxCapacity = 100    // Essentially unlimited

  // Internal game random number generator -- primarily for randomizing valid action list.
  val random = new Random(seed)

  /*
   * Cloning
   */

  // TODO: Not implemented
  def deepCopy():PeckingOrderGame = {
    println ("NOTE: deepCopy() not implemented -- returning a shallow copy")
    // Return
    return new PeckingOrderGame(locations, objectOrder, instructionBook, seed, generationProperties)
  }



  /*
   * Generation Properties
   */

  def getGenerationProperties():Map[String, Int] = return this.generationProperties


  /*
   * Task Description
   */
  def getTaskDescription():String = {
    return "Your task is to read and follow the instructions book found in the environment. Repeat this until the game is completed."
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
    if (numItemsInInventory >= inventoryMaxCapacity) {
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
    os.append("Inventory: \n")

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

      // Readable
      // NOTE: Unlike other tasks, for this task, the instructions can be read without picking them up
      if (obj.isReadable) {
        actionsOut.append( ("read " + obj.name, ACTION_READ, Array(obj)) )
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
   * Update the instruction book
   */

  def updateInstructionBook(): Unit = {
    val nextObjToPickUpIdx = this.getScore().scoreRaw.toInt
    if ((nextObjToPickUpIdx >= 0) && (nextObjToPickUpIdx < this.objectOrder.length)) {
      val nextObjToPickUp = this.objectOrder(nextObjToPickUpIdx)
      this.instructionBook.readText = "The current task is to take the " + nextObjToPickUp.name + "."
    } else {
      this.instructionBook.readText = "The task is completed."
    }

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
    // Special to this task: Update the instruction book based on the current task progress
    this.updateInstructionBook()

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


class PeckingOrderGameGenerator {
  val doorMaker = new DoorMaker()


  def mkEnvironment(r:Random, seed:Int, fold:String):(ArrayBuffer[Room], Array[FastObject], Instructions) = {
    val locations = new ArrayBuffer[Room]()

    // Add only a single location for this game (no map/multiple locations).
    val randLocationIdx = r.nextInt(11)
    if (randLocationIdx == 0) locations.append(new Kitchen(r, addKnife = false))
    if (randLocationIdx == 1) locations.append(new Pantry(r))
    if (randLocationIdx == 2) locations.append(new Corridor(r))
    if (randLocationIdx == 3) locations.append(new Bedroom(r))
    if (randLocationIdx == 4) locations.append(new Backyard(r))
    if (randLocationIdx == 5) locations.append(new LivingRoom(r))
    if (randLocationIdx == 6) locations.append(new Bathroom(r))
    if (randLocationIdx == 7) locations.append(new LaundryRoom(r))
    if (randLocationIdx == 8) locations.append(new Driveway(r))
    if (randLocationIdx == 9) locations.append(new Street(r))
    if (randLocationIdx == 10) locations.append(new Supermarket(r))

    // Generate the arithmetic problem (abstract), and the math problem object.
    val instructionBook = new Instructions()
    instructionBook.isMovable = false

    // Add the instructions to the location
    locations(0).addObject(instructionBook)

    // Make task objects
    val taskObjects = this.mkTaskObjects(r, fold)

    // Place objects
    val objectsToPlace = r.shuffle(taskObjects.toList).toArray
    // Find possible containers for objects
    val visibleObjects = locations(0).collectVisibleObjects()
    val validContainers = new ArrayBuffer[FastObject]
    for (obj <- visibleObjects) {
      if (obj.isContainer && obj.isOpen) validContainers.append(obj)
    }

    // Add the objects to the containers around this location
    for (obj <- objectsToPlace) {
      if (validContainers.length > 0) {
        // Add to a container
        val randContainer = validContainers( r.nextInt(validContainers.length) )
        randContainer.addObject(obj)
      } else {
        // Add to the environment (i.e. on the floor)
        locations(0).addObject(obj)
      }
    }

    return (locations, taskObjects, instructionBook)
  }


  // Make task objects
  def mkTaskObjects(r:Random, gameFold:String):Array[FastObject] = {
    val objectNamesTrain  = Array(("apple", "apples"), ("orange", "oranges"), ("grape", "grapes"), ("tangerine", "tangerines"), ("banana", "bananas"), ("pineapple", "pineapples"), ("papaya", "papayas"), ("peach", "peaches"), ("strawberry", "strawberries"), ("grapefruit", "grapefruits") )
    val objectNamesDev    = Array(("broccoli", "brocollis"), ("onion", "onions"), ("cucumber", "cucumbers"), ("potato", "potatoes"), ("cucumber", "cucumbers"), ("coconut", "coconuts"), ("watermelon", "watermelons"), ("mango", "mangos"), ("olive", "olives"), ("lime", "limes"), ("pear", "pears") )
    val objectNamesTest   = Array(("pepper", "peppers"), ("tomato", "tomatoes"), ("eggplant", "eggplants"), ("squash", "squashes"), ("pumpkin", "pumpkins"), ("pea", "peas"), ("avocado", "avocados"), ("cabbage", "cabbages"), ("prune", "prunes"), ("blueberry", "blueberries") )

    val numObjects:Int = 4

    // Find appropriate set, and shuffle order
    val objectNames:Array[(String, String)] = if (gameFold == "train") { objectNamesTrain } else if (gameFold == "dev") { objectNamesDev } else if (gameFold == "test") { objectNamesTest } else { Array.empty[(String, String)] }
    val shuffled = r.shuffle(r.shuffle(objectNames.toList)).toArray   // Double shuffle, so the order is different than other games

    // Create task objects, in order
    val objectsOut = new ArrayBuffer[FastObject]
    for (i <- 0 until numObjects) {
      val objName = shuffled(i)._1
      val obj = new FastObject(name = objName)
      obj.isMovable = true

      objectsOut.append(obj)
    }

    // Return
    return objectsOut.toArray
  }


  //def mkGame(seed:Long, numLocations:Int = 12, numDistractorItems:Int = 10, includeDoors:Boolean = true, limitInventorySize:Boolean = true, fold:String = "train"):CoinGame = {
  def mkGame(seed:Long, fold:String = "train"):PeckingOrderGame = {
    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1

    // Generate Game
    val r = new Random(seed)
    val (locations, taskObjects, instructionBook) = mkEnvironment(r, seed.toInt, fold)
    val game = new PeckingOrderGame( locations.toArray, taskObjects, instructionBook, seed, generationProperties = props.toMap )

    return game
  }


  def mkGameWithGoldPath(seed:Long, fold:String = "train"):(PeckingOrderGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 50
    val rg = new Random()

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, fold)
        val goldAgent = new PeckingOrderGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, fold)
    return (game, goldPath)
  }


}



