package textworldexpress.games

import textworldexpress.data.{LoadTWCDataJSON, LoadCookingWorldDataJSON, RecipeIngredient}
import textworldexpress.goldagent.CookingWorldGoldAgent
import textworldexpress.objects.{Backyard, Bathroom, Bedroom, Cookbook, Corridor, Counter, DoorMaker, Driveway, FastObject, Kitchen, Knife, LaundryRoom, LivingRoom, Meal, Pantry, Room, Street, Supermarket}
import textworldexpress.struct.{ActionHistory, GameScore, Scorer, StepResult, TextGame}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import scala.util.control.Breaks._


// 'recipe' and 'taskObjects' are parallel arrays representing the same objects
class CookingWorldGameScoring(val recipe:ArrayBuffer[RecipeIngredient], val taskObjects:ArrayBuffer[FastObject]) extends Scorer {
  var maxScoreFromPrep:Double = 0.0
  var ingredientsFound:Array[Boolean] = Array.fill(taskObjects.length) { false }

  var preparedMeal:Option[FastObject] = None
  def doScoring(preparedMeal:Option[FastObject]): Unit = {
    this.preparedMeal = preparedMeal
    this.doScoring()
  }

  // Should never call this directly -- must call the above, with the reference to preparedMeal.
  def doScoring(): Unit = {
    // Check status of each object
    var curScore:Double = 0
    var taskFailure:Boolean = false
    var taskSuccess:Boolean = false

    // Stage 1: Check the status of the ingredients
    if (this.preparedMeal.isEmpty) {
      for (i <- 0 until recipe.length) {
        //println ("TaskObject " + i + " : " + taskObjects(i).name)

        // Step 1: Check that important task object isn't deleted (failure)
        if (taskObjects(i).isDeleted) {
          //println ("Failure: " + taskObjects(i).name + " is deleted")
          taskFailure = true
        }

        // Step 2: Check that task object wasn't prepared incorrectly (failure)
        if (taskObjects(i).isPreparedIncorrectly(recipe(i).preparation)) {
          //println ("Failure: " + taskObjects(i).name + " is prepared incorrectly " + (recipe(i).preparation.mkString(", ")))
          taskFailure = true
        }

        // Step 3: Check that the task object has been in the agent's inventory at least once (success)
        if (ingredientsFound(i) || ((taskObjects(i).currentContainer != null) && (taskObjects(i).currentContainer.name == "inventory"))) {
          curScore += 1.0
          ingredientsFound(i) = true
        }

        // Step 4: Check that the task object was prepared correctly (success)
        if (recipe(i).requiresPrep) {
          curScore += taskObjects(i).scorePreparedCorrectly(recipe(i).preparation)
        }
      }

      maxScoreFromPrep = curScore
    } else {
      // Stage 2: Prepared meal is defined -- set minimum score to the number of ingredients plus one
      curScore = maxScoreFromPrep + 1.0

      // Check if the prepared meal was also eaten
      if (this.preparedMeal.get.isEaten) {
        curScore += 1.0
        taskSuccess = true
      }

    }

    // Store current scores in 'this.curScore', for the accessor in the base class
    val normalizedScore = curScore/maxScore
    val scores = new GameScore(scoreRaw = curScore, scoreNormalized = normalizedScore, taskSuccess = taskSuccess, taskFailure = taskFailure)
    this.curScore = scores

  }

  // Calculate the maximum possible score for this recipe
  def calculateMaxScore():Double = {
    var maxScore:Double = 0.0

    for (i <- 0 until recipe.length) {
      val ingredient = recipe(i).preparation

      // Each pick-up: 1 point
      maxScore += 1.0

      // Each chopping: 1 point
      if (!recipe(i).preparation.contains("uncut")) maxScore += 1.0

      // Each cooking: 1 point
      if (!recipe(i).preparation.contains("raw")) maxScore += 1.0
    }

    // Prepare meal and eat meal: 2 points
    maxScore += 2.0

    return maxScore
  }

}

class CookingWorldGame(val locations:Array[Room], val recipe:ArrayBuffer[RecipeIngredient], val taskObjects:ArrayBuffer[FastObject], limitInventorySize:Boolean, val seed:Long = 0, val generationProperties:Map[String, Int]) extends TextGame {

  // Inventory
  var agentInventory = new FastObject("inventory")

  // Initial location
  var agentLocation:Room = locations(0)

  // Objects that have been deleted from the environment, but whose status should still be tracked
  val deletedObjects = new ArrayBuffer[FastObject]()

  // Scorer
  val scorer = new CookingWorldGameScoring(recipe, taskObjects)
  var meal:Option[FastObject] = None    // Prepared meal (for scoring)

  // A list of the most recently generated valid actions (for step() )
  var lastValidActions = ListBuffer.empty[(String, Int, Array[FastObject])]

  // The action/observation history
  var history = new ArrayBuffer[ActionHistory]

  // Maximum capacity of the inventory (if limitInventorySize is enabled)
  val inventoryMaxCapacity = this.recipe.length + 2

  val random = new Random(seed)

  /*
   * Cloning
   */
  def deepCopy():TextGame = {
    println ("TODO: Cloning not implemented -- creating shallow copy.")

    return new CookingWorldGame(locations, recipe, taskObjects, this.limitInventorySize, seed=this.seed, this.generationProperties)
    /*
    val clonedTaskObjects = new ArrayBuffer[FastObject]

    // Step 1: Clone locations
    val locationsClone = new Array[Room](locations.length)
    for (i <- 0 until locations.length) {
      locationsClone(i) = locations(i).deepCopy(existingTaskObjects = taskObjects, copyTaskObjects = clonedTaskObjects)
    }

    // Step 2: Connect rooms
    this.connectClonedMap(locationsClone)

    // Step 3: Create new game
    val recipeCloned = new ArrayBuffer[RecipeIngredient]
    recipeCloned.insertAll(0, recipe)
    val game = new CookingWorldGame(locationsClone, recipeCloned, clonedTaskObjects, this.limitInventorySize, seed = this.seed)

    // Also clone the agent inventory
    game.agentInventory = this.agentInventory.deepCopy(existingTaskObjects = taskObjects, copyTaskObjects = clonedTaskObjects)

    // Return
    game
     */
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
    return "You are hungry! Let's cook a delicious meal. Check the cookbook in the kitchen for the recipe. Once done, enjoy your meal!"
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

  def agentHasKnife():Boolean = {
    for (obj <- agentInventory.contents) {
      if (obj.name == "knife") return true
    }
    return false
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
  def getScore():GameScore = this.scorer.getCurrentScore()

  override def getObjectTree():String = {

    val os = new StringBuilder()
    os.append("{")
    os.append("\"player_location\":\"" + this.agentLocation.name + "\",")
    //os.append("\"inventory\": " + this.agentInventory.toJSON() + ",")

    val inventoryJSON = new ArrayBuffer[String]()
    for (obj <- this.agentInventory.contents) {
      inventoryJSON.append("\"" + obj.name + "\": " + obj.toJSON)
    }
    os.append("\"inventory\": " + inventoryJSON.mkString("{", ",", "},"))

    val locations_json = new ArrayBuffer[String]()
    for (loc <- this.locations) {
      //locations_json.append(loc.toJSON())
      locations_json.append("\"" + loc.name + "\": " + loc.toJSON())
    }

    //os.append("\"locations\": " + locations_json.mkString("[", ",", "],"))
    os.append("\"locations\": " + locations_json.mkString("{", ",", "},"))

    val recipeJSON = new ArrayBuffer[String]()
    for (ingredient <- this.recipe) {
      recipeJSON.append("{\"name\": \"" + ingredient.name + "\", \"preparation\": \"" + ingredient.preparation.mkString(", ") + "\"}")
    }
    os.append("\"recipe\": " + recipeJSON.mkString("[", ",", "],"))

    val deletedObjectsJSON = new ArrayBuffer[String]()
    for (obj <- this.deletedObjects) {
      //deletedObjectsJSON.append(obj.toJSON())
      deletedObjectsJSON.append("\"" + obj.name + "\": " + obj.toJSON())
    }
    //os.append("\"deleted_objects\": " + deletedObjectsJSON.mkString("[", ",", "]"))
    os.append("\"deleted_objects\": " + deletedObjectsJSON.mkString("{", ",", "}"))

    os.append("}")
    return os.toString()
  }

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
    } else if (device.name == "barbeque" || device.name == "toaster") {
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
    if (obj.isReadable) {
      return this.actionRead(obj)
    }

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

  def canPrepareMeal():String = {
    if (agentLocation.name != "kitchen") {
      return "Can only prepare meal in the -= kitchen =-."
    }

    // Step 1: Check to see if the agent's inventory contains all the required ingredients, in their required forms
    var conditionsMet:Boolean = true
    for (ingredient <- this.taskObjects) {
      // Check in correct location (inventory)
      if ((ingredient.currentContainer == null) || (ingredient.currentContainer.name != "inventory")) {
        conditionsMet = false
      }

      // Check that it is prepared correctly
      var recipeItem:RecipeIngredient = null
      for (r <- this.recipe) if (r.name == ingredient.name) recipeItem = r      // Note, not even adding the break since the number of cycles here is very low (1-5)

      if (!ingredient.isPreparedCorrectly(recipeItem.preparation)) {
        conditionsMet = false
      }
    }

    if (!conditionsMet) {
      return "You are unable to prepare the meal right now."
    }

    return ""
  }

  def actionPrepareMeal(params:Array[FastObject]):String = this.actionPrepareMeal()
  def actionPrepareMeal():String = {
    var errMsg = this.canPrepareMeal()
    if (errMsg != "") {
      return errMsg
    }

    // Step 2: Consume ingredients, create 'meal'
    // Remove ingredients
    for (ingredient <- this.taskObjects) {
      ingredient.removeFromCurrentContainer()
      ingredient.isDeleted = true
    }

    // Step 3: Create 'meal'
    val preparedMeal = new Meal()
    this.meal = Some(preparedMeal)
    agentInventory.addObject(preparedMeal)

    return "Adding the meal to your inventory."
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
      case ACTION_EAT => return this.actionEat(params)
      case ACTION_COOK => return this.actionCook(params)
      case ACTION_CHOP => return this.actionChop(params)
      case ACTION_SLICE => return this.actionSlice(params)
      case ACTION_DICE => return this.actionDice(params)
      case ACTION_LOOKAROUND => return this.actionLookAround(params)
      case ACTION_MOVE => return this.actionMove(params)
      case ACTION_READ => return this.actionRead(params)
      case ACTION_PREPAREMEAL => return this.actionPrepareMeal(params)
      case ACTION_INVENTORY => return this.actionInventory(params)
      case ACTION_EXAMINE => return this.actionExamine(params)
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

    if (this.canPrepareMeal() == "") {
      actionsOut.append( ("prepare meal", ACTION_PREPAREMEAL, Array.empty[FastObject]) )
    }

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
      // Examine object
      actionsOut.append( ("examine " + obj.name, ACTION_EXAMINE, Array(obj)) )

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

      // Readable
      if (obj.isReadable) {
        actionsOut.append( ("read " + obj.name, ACTION_READ, Array(obj)) )
      }

    }


    val agentHasKnife = this.agentHasKnife()

    // Inventory objects
    for (iObj <- this.agentInventory.contents) {
      // Examine object
      actionsOut.append( ("examine " + iObj.name, ACTION_EXAMINE, Array(iObj)) )

      // For each environment object
      for (eObj <- visibleObjects) {
        if ((eObj.isContainer) && (eObj.isOpen)) {
          actionsOut.append( ("put " + iObj.name + " in " + eObj.name, ACTION_PUTIN, Array(iObj, eObj)) )
        }

        if (iObj.isCookable && eObj.isCookingDevice && !iObj.isFried && !iObj.isRoasted && !iObj.isGrilled) {
          actionsOut.append( ("cook " + iObj.name + " in " + eObj.name, ACTION_COOK, Array(iObj, eObj)) )
        }
      }

      if (iObj.isEdible) {
        actionsOut.append(("eat " + iObj.name, ACTION_EAT, Array(iObj)))
      }

      if (iObj.isCuttable && !iObj.isCut && agentHasKnife) {
        actionsOut.append( ("chop " + iObj.name, ACTION_CHOP, Array(iObj)) )
        actionsOut.append( ("slice " + iObj.name, ACTION_SLICE, Array(iObj)) )
        actionsOut.append( ("dice " + iObj.name, ACTION_DICE, Array(iObj)) )
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
    scorer.doScoring(this.meal)

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


class CookingWorldGameGenerator {
  val TWCObjectDatabase = new LoadTWCDataJSON()
  val CookingWorldObjectDatabase = new LoadCookingWorldDataJSON()
  val doorMaker = new DoorMaker()


  def mkEnvironment(r:Random, numLocations:Int, numDistractorItems:Int, numIngredients:Int, includeDoors:Boolean, fold:String):(ArrayBuffer[Room], ArrayBuffer[RecipeIngredient], ArrayBuffer[FastObject]) = {
    val locations = new ArrayBuffer[Room]()

    val kitchen = new Kitchen(r, addToaster=numLocations < 3)
    locations.append(kitchen)

    if (numLocations >= 2) locations.append( new Pantry(r) )
    if (numLocations >= 3) locations.append( new Backyard(r) )
    if (numLocations >= 4) locations.append( new Corridor(r) )
    if (numLocations >= 5) locations.append( new Bedroom(r) )
    if (numLocations >= 6) locations.append( new LivingRoom(r) )
    if (numLocations >= 7) locations.append( new Bathroom(r) )
    if (numLocations >= 8) locations.append( new LaundryRoom(r) )
    if (numLocations >= 9) locations.append( new Driveway(r) )
    if (numLocations >= 10) locations.append( new Street(r) )
    if (numLocations >= 11) locations.append( new Supermarket(r) )

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

    //## Debug
    //println("MAP:")
    //println(displayMap(map.get))



    // Randomly generate recipe
    val recipeIngredients = CookingWorldObjectDatabase.mkRandomRecipe(r, numIngredients, fold)

    // Add recipe ingredients to environment
    val taskObjects = this.addIngredientItems(r, locations, recipeIngredients)

    // Add distractor items
    val addedDistractorNames = this.addDistractorItems(r, locations, numToAdd = numDistractorItems, taskObjects)

    /*
    for (i <- 0 until recipeIngredients.length) {
      println (i + ": " + recipeIngredients(i).toString())
    }
     */


    // Make cookbook
    val cookbook = mkCookbook(recipeIngredients)

    // Add cookbook to kitchen counter
    val kitchenObjects = kitchen.contents
    breakable {
      for (obj <- kitchenObjects) {
        if (obj.name == "counter") {
          obj.addObject(cookbook)
          break()
        }
      }
    }

    return (locations, recipeIngredients, taskObjects)
  }

  def mkCookbook(recipeIngredients:ArrayBuffer[RecipeIngredient]):FastObject = {
    val os = new StringBuilder

    os.append("Gather all following ingredients and follow the directions to prepare this tasty meal.\n\n")

    os.append("Ingredients:\n")
    for (i <- 0 until recipeIngredients.length) {
      os.append("  "+ recipeIngredients(i).name + "\n")
    }

    os.append("\n")

    os.append("Directions:\n")
    for (i <- 0 until recipeIngredients.length) {
      // Cutting
      if (recipeIngredients(i).preparation.contains("chopped"))   os.append("  chop the " + recipeIngredients(i).name + "\n")
      if (recipeIngredients(i).preparation.contains("sliced"))    os.append("  slice the " + recipeIngredients(i).name + "\n")
      if (recipeIngredients(i).preparation.contains("diced"))     os.append("  dice the " + recipeIngredients(i).name + "\n")

      // Cooking
      if (recipeIngredients(i).preparation.contains("fried"))     os.append("  fry the " + recipeIngredients(i).name + "\n")
      if (recipeIngredients(i).preparation.contains("roasted"))   os.append("  roast the " + recipeIngredients(i).name + "\n")
      if (recipeIngredients(i).preparation.contains("grilled"))   os.append("  grill the " + recipeIngredients(i).name + "\n")
    }

    os.append("  prepare meal\n\n")


    // Make cookbook
    val book = new Cookbook()
    book.readText = os.toString()

    return book
  }

  // Add the ingredients of the recipe into the environment
  def addIngredientItems(r:Random, locations:ArrayBuffer[Room], recipeIngredients:ArrayBuffer[RecipeIngredient]):ArrayBuffer[FastObject] = {
    var attempts:Int = 0
    val objectNamesAdded = new ArrayBuffer[String]()

    // Collect all objects in the environment, and where they are
    // TODO: SLOW?
    val allObjects = mutable.Map[String, FastObject]()
    for (location <- locations) {
      val objs = location.collectVisibleObjects()
      for (obj <- objs) {
        allObjects(obj.name) = obj
      }
    }

    val objectsAdded = new ArrayBuffer[FastObject]
    // Place the recipe ingredients
    for (i <- 0 until recipeIngredients.length) {
      val ingredient = recipeIngredients(i)
      //println (CookingWorldObjectDatabase.lutObj.keySet.toArray.sorted.mkString(", "))
      val recipeItem = CookingWorldObjectDatabase.mkFastObjectByName(ingredient.name)
      val validLocations = recipeItem.canonicalLocations

      // Try to place the object in a random location
      breakable {
        var attempts: Int = 0
        while (attempts < 100) {
          val randLocationIdx = r.nextInt(validLocations.length)
          val generationLocation = validLocations(randLocationIdx)
          if (allObjects.contains(generationLocation)) {
            allObjects(generationLocation).addObject(recipeItem)
            objectsAdded.append(recipeItem)
            //println ("Added " + recipeItem.name + " to " + generationLocation)
            break()
          }
          attempts += 1
        }
        // If we reach here, the ingredient was not able to be placed for some reason.
        // The most common reason is that the ingredient lists canonical locations (e.g. garden) that are not in the
        // current environment, likely beacause a small game without that location was generated.
        // In this case, we'll just randomly select a location to dump the ingredient in the smaller environment, and move on.
        val backupLocations = r.shuffle(List("fridge", "counter", "kitchen cupboard", "dining table"))
        for (backupLocation <- backupLocations) {
          if (allObjects.contains(backupLocation)) {
            allObjects(backupLocation).addObject(recipeItem)
            objectsAdded.append(recipeItem)
            //println ("Backup: Placing " + recipeItem.name + " in " + backupLocation)
            break()
          }
        }

        // If we reach here, something is really wrong -- throw an exception.
        throw new RuntimeException("ERROR: Unable to place recipe ingredient: " + recipeItem.toString)
      }

    }

    return objectsAdded
  }

  // Randomly add some number of distractor items to the environment, in their canonical locations, using the TWC database.
  def addDistractorItems(r:Random, locations:ArrayBuffer[Room], numToAdd:Int, taskObjects:ArrayBuffer[FastObject]): ArrayBuffer[String] = {
    val objectNamesAdded = new ArrayBuffer[String]()
    for (taskObject <- taskObjects) objectNamesAdded.append(taskObject.name)

    var attempts:Int = 0
    while ((objectNamesAdded.length < numToAdd+taskObjects.length) && (attempts < 100)) {
      // Step 1: Pick a random location
      val randLocIdx:Int = r.nextInt(locations.length)
      val location = locations(randLocIdx)
      val objects = location.contents.toArray.sortBy(_.name)    // TODO: EXPENSIVE
      if (objects.length > 0) {
        val randObjIdx = r.nextInt(objects.length)
        val container = objects(randObjIdx)

        //println("location: " + location.name)

        //val distractorItem = TWCObjectDatabase.mkRandomObjectByLocation(r, container.name)
        val distractorItem = CookingWorldObjectDatabase.mkRandomObjectByLocation(r, container.name)
        if (distractorItem.isDefined) {
          if (!objectNamesAdded.contains(distractorItem.get.name)) {
            container.addObject(distractorItem.get)
            objectNamesAdded.append(distractorItem.get.name)
            //println ("Added " + distractorItem.get.name + " to " + container.name + " in " + location.name)
          }
        } else {
          //println ("distractor not defined")
        }
      }

      attempts += 1
      //println ("Attempts: " + attempts)
    }

    return objectNamesAdded
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

    os.append("Connections:")
    for (i <- 0 until map.size) {
      for (j <- 0 until map(i).size) {
        val cell = map(i)(j)
        if (cell != null) {
          if (cell.locationNorth != null) os.append(cell.name + "-> " + cell.locationNorth.name + "\n")
          if (cell.locationSouth != null) os.append(cell.name + "-> " + cell.locationSouth.name + "\n")
          if (cell.locationEast != null) os.append(cell.name + "-> " + cell.locationEast.name + "\n")
          if (cell.locationWest != null) os.append(cell.name + "-> " + cell.locationWest.name + "\n")
        }
      }
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


  def mkGame(seed:Long, numLocations:Int = 12, numDistractorItems:Int = 10, numIngredients:Int = 3, includeDoors:Boolean = true, limitInventorySize:Boolean = true, fold:String = "train"):CookingWorldGame = {
    // Store properties in a form that are user accessible later on
    val props = mutable.Map[String, Int]()
    props("seed") = seed.toInt
    props("numLocations") = numLocations
    props("numDistractorItems") = numDistractorItems
    props("numIngredients") = numIngredients
    props("includeDoors") = if (includeDoors) { 1 } else { 0 }
    props("limitInventorySize") = if (limitInventorySize) { 1 } else { 0 }
    props("gameSet") = if (fold == "train") { 1 } else if (fold == "dev") { 2 } else if (fold == "test") { 3 } else -1

    // Generate Game
    val r = new Random(seed)
    val (locations, recipe, taskObjects) = mkEnvironment(r, numLocations, numDistractorItems, numIngredients, includeDoors, fold)
    val game = new CookingWorldGame( locations.toArray, recipe, taskObjects, limitInventorySize, generationProperties = props.toMap )

    return game
  }


  def mkGameWithGoldPath(seed:Long, numLocations:Int = 12, numDistractorItems:Int = 10, numIngredients:Int = 3, includeDoors:Boolean = true, limitInventorySize:Boolean = true, fold:String = "train"):(CookingWorldGame, Array[String]) = {
    val MAX_ATTEMPTS:Int = 50
    val rg = new Random(seed)

    var attempts: Int = 0
    var goldPath = Array.empty[String]
    breakable {
      while (attempts < MAX_ATTEMPTS) {
        val game = this.mkGame(seed, numLocations, numDistractorItems, numIngredients, includeDoors, limitInventorySize, fold)
        val goldAgent = new CookingWorldGoldAgent(game)
        val (success, _goldPath) = goldAgent.mkGoldPath(rg)
        if (success) goldPath = _goldPath

        if (success) break()
        attempts += 1
      }

      // If we reach here, for some reason no gold path could be generated.
      println ("ERROR: Unknown error: Gold path could not be generated after maximum number of attempts (" + MAX_ATTEMPTS + ").")
    }

    // Create fresh copy of game
    val game = this.mkGame(seed, numLocations, numDistractorItems, numIngredients, includeDoors, limitInventorySize, fold)
    return (game, goldPath)
  }


}
