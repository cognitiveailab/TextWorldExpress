package textworldexpress.objects

import textworldexpress.JSON

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

/*
 * Base object
 */
class FastObject(val name:String) {
  var isContainer:Boolean = false
  var isOpen:Boolean = false
  var isOpenable:Boolean = false
  var isLocation:Boolean = false
  var isCookingDevice:Boolean = false

  var isMovable:Boolean = false

  // Generic eating
  var isEdible:Boolean = false
  var isDrinkable:Boolean = false
  // Cutting
  var isCuttable:Boolean = false
  var isCut:Boolean = false
  var isChopped:Boolean = false
  var isSliced:Boolean = false
  var isDiced:Boolean = false
  // Cooking
  var isCookable:Boolean = false
  var needsCooking:Boolean = false
  var isRaw:Boolean = false
  var isFried:Boolean = false
  var isRoasted:Boolean = false
  var isGrilled:Boolean = false

  // Readability:Boolean = false
  var isReadable:Boolean = false
  var readText:String = ""
  var hasBeenRead:Boolean = false

  var isDeleted:Boolean = false
  var isEaten:Boolean = false


  // Indefinite
  var indefinite = if (Array("a", "e", "i", "o", "u").contains(this.name.charAt(0).toString)) { "an" } else { "a" }

  var canonicalLocations:Array[String] = Array.empty[String]
  var prepositionReferent:String = "in "

  var currentContainer:FastObject = null
  val contents = mutable.ArrayBuffer[FastObject]()


  // Attempt at a deep copy
  def deepCopy(existingTaskObjects:ArrayBuffer[FastObject], copyTaskObjects:ArrayBuffer[FastObject]):FastObject = {
    val out = new FastObject(name)
    out.isContainer = this.isContainer
    out.isOpen = this.isOpen
    out.isOpenable = this.isOpenable
    out.isLocation = this.isLocation
    out.isCookingDevice = this.isCookingDevice

    out.isMovable = this.isMovable
    out.isEdible = this.isEdible
    out.isDrinkable = this.isDrinkable
    out.isCuttable = this.isCuttable
    out.isCut = this.isCut
    out.isChopped = this.isChopped
    out.isSliced = this.isSliced
    out.isDiced = this.isDiced

    out.isCookable = this.isCookable
    out.needsCooking = this.needsCooking
    out.isRaw = this.isRaw
    out.isFried = this.isFried
    out.isRoasted = this.isRoasted
    out.isGrilled = this.isGrilled

    out.isReadable = this.isReadable
    out.readText = this.readText

    out.isDeleted = this.isDeleted
    out.isEaten = this.isEaten

    out.indefinite = this.indefinite                    // OK to be a shallow copy?
    out.canonicalLocations = this.canonicalLocations    // OK to be a shallow copy?
    out.prepositionReferent = this.prepositionReferent

    // Current Container
    // Do not set -- set it automatically from cloning

    // Contents
    // Deep copy all contents
    for (cObj <- this.contents) {
      val clonedObject = cObj.deepCopy(existingTaskObjects, copyTaskObjects)
      out.addObject( clonedObject )

      // Check to see if this object should be on the task objects list
      for (tObj <- existingTaskObjects) {
        if (cObj.name == tObj.name) {
          copyTaskObjects.append(clonedObject)
        }
      }
    }

    // Return
    out
  }


  /*
   * Accessors
   */

  // Get all objects contained by this object, and all it's (open, accessible) containers
  def collectVisibleObjects(objsIn:ListBuffer[FastObject] = new ListBuffer[FastObject]):ListBuffer[FastObject] = {
    for (obj <- this.contents) {
      objsIn.append(obj)

      if ((obj.isLocation) || (obj.isContainer && obj.isOpen)) {
        obj.collectVisibleObjects(objsIn)
      }
    }
    return objsIn
  }

  def addObject(obj:FastObject): Unit = {
    // Remove from old container
    if (obj.currentContainer != null) {
      obj.removeFromCurrentContainer()
    }

    // Put in new container
    this.contents.append(obj)
    obj.currentContainer = this
  }

  def removeObject(obj:FastObject): Unit = {
    val result = this.contents -= obj
    //println ("Removing " + obj.name + " from " + this.name + " (" + result + ")")

    obj.currentContainer = null
  }

  // Remove the object from it's current container
  def removeFromCurrentContainer(): Unit = {
    if (this.currentContainer != null) {
      this.currentContainer.removeObject(this)
    }
  }

  // Check to see if the ingredient is prepared correctly (i.e. meets conditions for being turned into the 'meal')
  def isPreparedCorrectly(requirements:Set[String]):Boolean = {
    /*
    for (requirement <- requirements) {
      requirement match {
        case "raw" =>     if (!this.isRaw)      { println("*raw"); return false }
        case "roasted" => if (!this.isRoasted)  { println("*roasted"); return false }
        case "fried" =>   if (!this.isFried)    { println("*fried"); return false }
        case "grilled" => if (!this.isGrilled)  { println("*grilled"); return false }

        case "uncut" =>   if (this.isCut)       { println("*uncut"); return false }
        case "chopped" => if (!this.isChopped)  { println("*chopped"); return false }
        case "diced" =>   if (!this.isDiced)    { println("*diced"); return false }
        case "sliced" =>  if (!this.isSliced)    { println("*sliced"); return false }
      }
    }
     */
    for (requirement <- requirements) {
      requirement match {
        case "raw" =>     if (!this.isRaw)      { return false }
        case "roasted" => if (!this.isRoasted)  { return false }
        case "fried" =>   if (!this.isFried)    { return false }
        case "grilled" => if (!this.isGrilled)  { return false }

        case "uncut" =>   if (this.isCut)       { return false }
        case "chopped" => if (!this.isChopped)  { return false }
        case "diced" =>   if (!this.isDiced)    { return false }
        case "sliced" =>  if (!this.isSliced)   { return false }
      }
    }


    return true
  }

  def scorePreparedCorrectly(requirements:Set[String]):Double = {
    var score:Double = 0.0

    for (requirement <- requirements) {
      requirement match {
        //case "raw" =>     if (this.isRaw)     score += 1.0      // No score for properties that don't require preparation actions
        case "roasted" => if (this.isRoasted) score += 1.0
        case "fried" =>   if (this.isFried)   score += 1.0
        case "grilled" => if (this.isGrilled) score += 1.0

        //case "uncut" =>   if (this.isCut)     score += 1.0      // No score for properties that don't require preparation actions
        case "chopped" => if (this.isChopped) score += 1.0
        case "diced" =>   if (this.isDiced)   score += 1.0
        case "sliced" =>  if (this.isSliced)  score += 1.0

        case _ => { // do nothing
                  }
      }
    }

    return score
  }

  // Check to see if the ingredient is prepared incorrectly (i.e. is in a failure condition)
  def isPreparedIncorrectly(requirements:Set[String]):Boolean = {

    for (requirement <- requirements) {
      requirement match {
        case "raw" => if (!this.isRaw) return true
        case "roasted" => if (this.isFried || this.isGrilled) return true
        case "fried" => if (this.isGrilled || this.isRoasted) return true
        case "grilled" => if (this.isFried || this.isRoasted) return true

        case "uncut" => if (this.isCut) return true
        case "chopped" => if (this.isDiced || this.isSliced) return true
        case "diced" => if (this.isChopped || this.isSliced) return true
        case "sliced" => if (this.isChopped || this.isDiced) return true
      }
    }

    return false
  }

  def getFoodAdjectives():String = {
    val os = new mutable.StringBuilder()

    if (this.isRaw && this.needsCooking) os.append("raw ")
    if (this.isFried) os.append("fried ")
    if (this.isRoasted) os.append("roasted ")
    if (this.isGrilled) os.append("grilled ")

    if (this.isChopped) os.append("chopped ")
    if (this.isSliced) os.append("sliced ")
    if (this.isDiced) os.append("diced ")

    return os.toString()
  }

  // Make a human-readable contents string (e.g. "a carrot, a cucumber, and an apple")
  def getContentsListStr():String = {
    val numContainedItems:Int = this.contents.size
    val contentsArray = this.contents.toArray

    if (numContainedItems == 0) {
      return "nothing"
    } else if (numContainedItems == 1) {
      return contentsArray(0).getDescription()
    } else {
      val os = new StringBuilder
      for (i <- 0 until numContainedItems-1) {
        os.append( contentsArray(i).getDescription() + ", " )
      }
      os.append("and " + contentsArray.last.getDescription())
      return os.toString()
    }
  }

  def getDescription():String = {
    return this.indefinite + " " + this.getFoodAdjectives() + this.name
  }

  def toJSON():String = {
    val os = new StringBuilder()
    os.append("{")
    os.append("\"name\":\"" + this.name + "\",")
    os.append("\"isContainer\":" + this.isContainer + ",")
    os.append("\"isOpen\":" + this.isOpen + ",")
    os.append("\"isOpenable\":" + this.isOpenable + ",")
    os.append("\"isLocation\":" + this.isLocation + ",")
    os.append("\"isCookingDevice\":" + this.isCookingDevice + ",")
    os.append("\"isMovable\":" + this.isMovable + ",")
    os.append("\"isEdible\":" + this.isEdible + ",")
    os.append("\"isDrinkable\":" + this.isDrinkable + ",")
    os.append("\"isCuttable\":" + this.isCuttable + ",")
    os.append("\"isCut\":" + this.isCut + ",")
    os.append("\"isChopped\":" + this.isChopped + ",")
    os.append("\"isSliced\":" + this.isSliced + ",")
    os.append("\"isDiced\":" + this.isDiced + ",")
    os.append("\"isCookable\":" + this.isCookable + ",")
    os.append("\"needsCooking\":" + this.needsCooking + ",")
    os.append("\"isRaw\":" + this.isRaw + ",")
    os.append("\"isFried\":" + this.isFried + ",")
    os.append("\"isRoasted\":" + this.isRoasted + ",")
    os.append("\"isGrilled\":" + this.isGrilled + ",")
    os.append("\"isReadable\":" + this.isReadable + ",")
    os.append("\"readText\":\"" + JSON.sanitize(this.readText) + "\",")
    os.append("\"isDeleted\":" + this.isDeleted + ",")
    os.append("\"isEaten\":" + this.isEaten + ",")

    // Contents
    val contents_json = new ArrayBuffer[String]()
    for (obj <- this.contents) {
      // contents_json.append(obj.toJSON())
      contents_json.append("\"" + obj.name + "\": " + obj.toJSON())
    }

    // os.append("\"contents\": " + contents_json.mkString("[", ",", "]"))
    os.append("\"contents\": " + contents_json.mkString("{", ",", "}"))
    os.append("}")
    return os.toString()
  }
}


/*
 * Rooms
 */
class Room(name:String) extends FastObject(name) {
  this.isLocation = true
  var prefersConnectingTo:Array[String] = Array.empty[String]     // Names of locations this room prefers to connect to
  val starts = Array("In one part of the room you see ", "There is also ", "You also see ", "In another part of the room you see ")

  var locationNorth:Room = null
  var locationSouth:Room = null
  var locationEast:Room = null
  var locationWest:Room = null

  var doorNorth:Door = null
  var doorSouth:Door = null
  var doorEast:Door = null
  var doorWest:Door = null


  // Attempt at a deep copy
  override def deepCopy(existingTaskObjects:ArrayBuffer[FastObject], copyTaskObjects:ArrayBuffer[FastObject]):Room = {
    // Door never contains anything or has any other properties, so cloning is cheap.
    val out = new Room(this.name)
    out.prefersConnectingTo = this.prefersConnectingTo    // Shallow copy OK?

    // Contents
    // Deep copy all contents
    for (cObj <- this.contents) {
      //##
      //val containerName = if (this.currentContainer == null) "null" else this.currentContainer.name
      //println ("\t Cloning (" + containerName + "): " + cObj.name )

      val clonedObject = cObj.deepCopy(existingTaskObjects, copyTaskObjects)
      out.addObject( clonedObject )

      // Check to see if this object should be on the task objects list
      for (tObj <- existingTaskObjects) {
        if (cObj.name == tObj.name) {
          copyTaskObjects.append(clonedObject)
        }
      }
    }

    /*
    // Doors -- can deep copy these
    if (this.doorNorth != null) out.doorNorth = this.doorNorth.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.doorSouth != null) out.doorSouth = this.doorSouth.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.doorEast != null) out.doorEast = this.doorEast.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.doorWest != null) out.doorWest = this.doorWest.deepCopy(existingTaskObjects, copyTaskObjects)
     */

    /*
    // Locations: Deep copy these (recursively)
    if (this.locationNorth != null) out.locationNorth = this.locationNorth.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.locationSouth != null) out.locationSouth = this.locationSouth.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.locationEast != null) out.locationEast = this.locationEast.deepCopy(existingTaskObjects, copyTaskObjects)
    if (this.locationWest != null) out.locationWest = this.locationWest.deepCopy(existingTaskObjects, copyTaskObjects)
     */

    // Return
    out
  }



  private def mkDirectionDescription(location:Room, door:Door, directionName:String):String = {
    if (location != null) {
      if (door == null) {
        return("To the " + directionName + " you see the " + location.name + ". ")
      } else {
        if (door.isOpen) {
          return("Through an open " + door.getDescription() + ", to the " + directionName + " you see the " + location.name + ". ")
        } else {
          return("To the " + directionName + " you see a closed " + door.getDescription() + ". ")
        }
      }
    }
    return ""
  }

  override def getDescription():String = {
    val os = new StringBuilder()
    os.append("You are in the " + name + ". ")

    val objDescriptions = contents.map(_.getDescription()).toList
    for (i <- 0 until objDescriptions.size) {
      os.append( starts(i%starts.length) )
      os.append( objDescriptions(i) + ". ")
    }

    os.append("\n")
    os.append( this.mkDirectionDescription(locationNorth, doorNorth, "North") )
    os.append( this.mkDirectionDescription(locationSouth, doorSouth, "South") )
    os.append( this.mkDirectionDescription(locationEast, doorEast, "East") )
    os.append( this.mkDirectionDescription(locationWest, doorWest, "West") )

    return os.toString()
  }

  override def toJSON():String = {
    val os = new StringBuilder(super.toJSON())
    os.update(os.length()-1, ',')  // replace '}' with ','

    if (this.locationNorth != null)
      os.append("\"locationNorth\": \"" + this.locationNorth.name + "\",")
    else
      os.append("\"locationNorth\": null,")

    if (this.locationSouth != null)
      os.append("\"locationSouth\": \"" + this.locationSouth.name + "\",")
    else
      os.append("\"locationSouth\": null,")

    if (this.locationEast != null)
      os.append("\"locationEast\": \"" + this.locationEast.name + "\",")
    else
      os.append("\"locationEast\": null,")

    if (this.locationWest != null)
      os.append("\"locationWest\": \"" + this.locationWest.name + "\",")
    else
      os.append("\"locationWest\": null,")

    if (this.doorNorth != null)
      os.append("\"doorNorth\":" + this.doorNorth.toJSON() + ",")
    else
      os.append("\"doorNorth\": null,")

    if (this.doorSouth != null)
      os.append("\"doorSouth\":" + this.doorSouth.toJSON() + ",")
    else
      os.append("\"doorSouth\": null,")

    if (this.doorEast != null)
      os.append("\"doorEast\":" + this.doorEast.toJSON() + ",")
    else
      os.append("\"doorEast\": null,")

    if (this.doorWest != null)
      os.append("\"doorWest\":" + this.doorWest.toJSON())
    else
      os.append("\"doorWest\": null")

    os.append("}")
    return os.toString()
  }

}

class Kitchen(r:Random, addKnife:Boolean = true, addToaster:Boolean = false) extends Room("kitchen") {
  this.prefersConnectingTo = Array("corridor", "pantry", "backyard", "living room")

  // Constructor
  this.init()

  def init(): Unit ={
    // Required
    this.addObject( new Stove() )
    this.addObject( new Oven() )

    // Optional. Add toaster (i.e., when there's no Backyard)
    if (addToaster) {
      this.addObject( new Toaster() )
    }

    val fridge = new Fridge()
    this.addObject(fridge)

    // Add counter
    val counter = new Counter()
    this.addObject(counter)

    // Add some kind of other surface
    var surface:FastObject = null
    val randInt1 = r.nextInt(2)
    if (randInt1 == 0) {
      surface = new DiningTable()
    } else {
      surface = new KitchenCupboard()
    }
    this.addObject(surface)

    // TODO: Add any requested objects to surface

    val cutleryDrawer = new CutleryDrawer()
    this.addObject(cutleryDrawer)

    // Add knife
    if (addKnife) {
      val randInt2 = r.nextInt(3)
      if (randInt2 == 0) {
        surface.addObject(new Knife())
      } else if (randInt2 == 1) {
        counter.addObject(new Knife())
      } else {
        cutleryDrawer.addObject(new Knife())
      }
    }

    this.addObject(new TrashCan() )
    this.addObject(new Dishwasher() )
    this.addObject(new ChairDining() )

  }

}


class Backyard(r:Random) extends Room("backyard") {
  this.prefersConnectingTo = Array("corridor", "kitchen", "living room", "sideyard", "alley")

  // Constructor
  this.init()

  def init(): Unit = {

    this.addObject( new BBQ() )
    this.addObject( new Workbench() )
    this.addObject( new PatioChair() )
    this.addObject( new PatioTable() )
    this.addObject( new ClothesLine() )
    this.addObject( new Garden() )
  }

}

class Pantry(r:Random) extends Room("pantry") {
  this.prefersConnectingTo = Array("kitchen")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new ChairFolding() )
    this.addObject(new Shelf() )
  }

}

class LaundryRoom(r:Random) extends Room("laundry room") {
  this.prefersConnectingTo = Array("corridor", "kitchen", "bathroom")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new Bench() )
    this.addObject(new WorkTable() )
    this.addObject(new ClothesDrier() )
    this.addObject(new LaundryBasket() )
    this.addObject(new WashingMachine() )
  }

}

class Bedroom(r:Random) extends Room("bedroom") {
  this.prefersConnectingTo = Array("corridor", "living room")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new DressingTable() )
    this.addObject(new ChairDesk() )
    this.addObject(new Desk() )
    this.addObject(new ChestOfDrawers() )
    this.addObject(new Wardrobe() )
    this.addObject(new NightStand() )
    this.addObject(new Bed() )
  }

}

class Bathroom(r:Random) extends Room("bathroom") {
  this.prefersConnectingTo = Array("corridor", "kitchen", "bedroom", "living room")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new DressingTable() )
    this.addObject(new Sink() )
    this.addObject(new WallHook() )
    this.addObject(new BathMat() )
    this.addObject(new ToiletRollHolder() )
    this.addObject(new TowelRack() )
    this.addObject(new Bathtub() )
    this.addObject(new Shower() )
    this.addObject(new TrashCan() )
    this.addObject(new BathroomCabinet() )
    this.addObject(new Toilet() )
  }

}

class Corridor(r:Random) extends Room("corridor") {
  this.prefersConnectingTo = Array("kitchen", "bathroom", "backyard", "laundry room", "bedroom", "living room", "foyer", "garage")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new KeyHolder() )
    this.addObject(new ShoeCabinet() )
    this.addObject(new UmbrellaStand() )
    this.addObject(new HatRack() )
    this.addObject(new CoatHanger() )
  }

}

class LivingRoom(r:Random) extends Room("living room") {
  this.prefersConnectingTo = Array("kitchen", "bathroom", "backyard", "bedroom", "foyer")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new TrashCan2() )
    this.addObject(new BookCase() )
    this.addObject(new TVStand() )
    this.addObject(new ChairArm() )
    this.addObject(new SideTable() )
    this.addObject(new CoffeeTable() )
    this.addObject(new EndTable() )
    this.addObject(new Sofa() )

  }

}

class Driveway(r:Random) extends Room("driveway") {
  this.prefersConnectingTo = Array("corridor", "backyard", "sideyard", "alley", "garage")

  // Constructor
  this.init()

  def init(): Unit = {

  }

}

class Street(r:Random) extends Room("street") {
  this.prefersConnectingTo = Array("driveway", "backyard", "supermarket", "alley")

  // Constructor
  this.init()

  def init(): Unit = {

  }

}

class Supermarket(r:Random) extends Room("supermarket") {
  this.prefersConnectingTo = Array("street", "alley")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject(new Showcase() )
  }

}

class Sideyard(r:Random) extends Room("sideyard") {
  this.prefersConnectingTo = Array("backyard", "driveway")

  // Constructor
  this.init()

  def init(): Unit = {
    this.addObject( new PatioChair() )
    this.addObject( new PatioTable() )
    this.addObject( new Garden() )
  }

}

class Alley(r:Random) extends Room("alley") {
  this.prefersConnectingTo = Array("backyard", "driveway", "street", "supermarket")

  // Constructor
  this.init()

  def init(): Unit = {
  }

}

class Foyer(r:Random) extends Room("foyer") {
  this.prefersConnectingTo = Array("livingroom", "corridor")

  // Constructor
  this.init()

  def init(): Unit = {
  }

}

class Garage(r:Random) extends Room("garage") {
  this.prefersConnectingTo = Array("driveway", "corridor")

  // Constructor
  this.init()

  def init(): Unit = {
  }

}

/*
 * Devices
 */
class Fridge() extends FastObject("fridge") {
  this.isContainer = true
  this.isOpen = false
  this.isOpenable = true

  override def getDescription():String = {
    if (this.isOpen == false) {
      return "a fridge that is closed"
    } else {
      if (this.contents.isEmpty) {
        return "an open fridge, that is empty"
      }

      val os = new StringBuilder()
      os.append("An open fridge, that contains ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())

      return os.toString()
    }
  }

}


// Stove
class Stove() extends FastObject("stove") {
  this.isCookingDevice = true
  this.prepositionReferent = "on "

  override def getDescription():String = {
      return "a stove"
  }
}

// Oven
class Oven() extends FastObject("oven") {
  this.isCookingDevice = true

  override def getDescription():String = {
    return "an oven"
  }
}

// BBQ
class BBQ() extends FastObject("barbeque") {
  this.isCookingDevice = true

  override def getDescription():String = {
    return "a barbeque"
  }
}

// Toaster
class Toaster() extends FastObject("toaster") {
  this.isCookingDevice = true

  override def getDescription():String = {
    return "a toaster"
  }
}

// Knife
class Knife() extends FastObject("knife") {
  this.isMovable = true

  override def getDescription():String = {
    return "a knife"
  }
}


/*
 * Cook book
 */
class Cookbook() extends FastObject("cookbook") {
  this.isReadable = true
  this.isMovable = true

}

// It's called 'Mapbook' because 'Map' is a standard collection name
class Mapbook() extends FastObject(name = "map") {
  this.isReadable = true
  this.isMovable = true

}

// A math problem, for Arithmetic Game
class MathProblem() extends FastObject(name = "math problem") {
  this.isReadable = true
  this.isMovable = true

}

// A set of instructions for what to do
class Instructions() extends FastObject(name = "instructions book") {
  this.isReadable = true
  this.isMovable = true

}

/*
 * Answer box
 */
class Box() extends FastObject("box") {
  this.isContainer = true
  this.isOpen = true
  this.isOpenable = false
  this.isMovable = false

  override def getDescription():String = {
    if (this.contents.isEmpty) {
      return "a box, that is empty"
    }

    val os = new StringBuilder()
    os.append("A box, that contains ")
    os.append(this.getContentsListStr())

    return os.toString()
  }

}

/*
 * Generic furniture objects
 */
// Generic for anything type of openable furniture
class GenericOpenableFurniture(name:String) extends FastObject(name) {
  this.isContainer = true
  this.isOpen = false
  this.isOpenable = true

  override def getDescription():String = {
    if (this.isOpen == false) {
      return "a " + name + " that is closed"
    } else {
      if (this.contents.isEmpty) {
        return "an open " + name + ", that is empty"
      }

      val os = new StringBuilder()
      os.append("An open " + name + ", that contains ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())

      return os.toString()
    }
  }

}

// Generic for anything type of openable furniture
class GenericSurfaceFurniture(name:String) extends FastObject(name) {
  this.isContainer = true
  this.isOpen = true
  this.isOpenable = false
  this.prepositionReferent = "on "

  override def getDescription():String = {
    if (this.contents.isEmpty) {
      return "a " + name + ", that has nothing on it"
    } else {
      val os = new StringBuilder()
      os.append("a " + name + " that has ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())
      os.append(" on it")
      return os.toString()
    }
  }

}

// Generic for anything unmovable
class GenericUnmovableFurniture(name:String) extends FastObject(name) {
  this.isContainer = false
  this.isOpen = false
  this.isOpenable = false
  this.isMovable = false

  override def getDescription():String = {
    if (this.contents.isEmpty) {
      return "a " + name + ", that has nothing on it"
    } else {
      val os = new StringBuilder()
      os.append("a " + name + " that has ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())
      os.append(" on it")
      return os.toString()
    }
  }

}

class GenericUnmovableFurnitureWithSurface(name:String) extends FastObject(name) {
  this.isContainer = true
  this.isOpen = true
  this.isOpenable = false
  this.isMovable = false

  override def getDescription():String = {
    if (this.contents.isEmpty) {
      return "a " + name + ", that has nothing on it"
    } else {
      val os = new StringBuilder()
      os.append("a " + name + " that has ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())
      os.append(" on it")
      return os.toString()
    }
  }

}

// Generic for anything movable
class GenericMovableFurniture(name:String) extends FastObject(name) {
  this.isContainer = false
  this.isOpen = false
  this.isOpenable = false
  this.isMovable = false

  override def getDescription():String = {
    if (this.contents.isEmpty) {
      return "a " + name + ", that has nothing on it"
    } else {
      val os = new StringBuilder()
      os.append("a " + name + " that has ")
      //os.append( contents.map(_.getDescription()).mkString(", ") )
      os.append(this.getContentsListStr())
      os.append(" on it")
      return os.toString()
    }
  }

}




/*
 * Cannonical containers/surfaces
 */
class Counter() extends GenericSurfaceFurniture("counter") {

}

class DiningTable() extends GenericSurfaceFurniture("dining table") {

}

class KitchenCupboard() extends GenericOpenableFurniture("kitchen cupboard") {

}

class Dishwasher() extends GenericOpenableFurniture("dishwasher") {

}

class CutleryDrawer() extends GenericOpenableFurniture("cutlery drawer") {

}

class TrashCan() extends GenericOpenableFurniture("trash can") {

}

class TrashCan2() extends GenericOpenableFurniture("wastepaper basket") {

}


/*
 * Other places to put things in a house
 */

class ChairLadderback() extends GenericUnmovableFurnitureWithSurface("ladderback chair") {

}

class ChairDining() extends GenericUnmovableFurnitureWithSurface("dining chair") {

}

class ChairArm() extends GenericUnmovableFurnitureWithSurface("arm chair") {

}

class ChairDesk() extends GenericUnmovableFurnitureWithSurface("desk chair") {

}

class ChairFolding() extends GenericUnmovableFurnitureWithSurface("folding chair") {

}

class Sofa() extends GenericUnmovableFurnitureWithSurface("sofa") {

}

class EndTable() extends GenericUnmovableFurnitureWithSurface("end table") {

}

class CoffeeTable() extends GenericUnmovableFurnitureWithSurface("coffee table") {

}

class SideTable() extends GenericUnmovableFurnitureWithSurface("side table") {

}

class BookCase() extends GenericUnmovableFurnitureWithSurface("book case") {

}

class TVStand() extends GenericUnmovableFurnitureWithSurface("TV stand") {

}


class CoatHanger() extends GenericUnmovableFurnitureWithSurface("coat hanger") {

}

class HatRack() extends GenericUnmovableFurnitureWithSurface("hat rack") {

}

class UmbrellaStand() extends GenericUnmovableFurnitureWithSurface("umbrella stand") {

}

class ShoeCabinet() extends GenericOpenableFurniture("shoe cabinet") {

}

class Wardrobe() extends GenericOpenableFurniture("wardrobe") {

}

class KeyHolder() extends GenericUnmovableFurnitureWithSurface("key holder") {

}

class Bed() extends GenericUnmovableFurnitureWithSurface("bed") {

}

class NightStand() extends GenericUnmovableFurnitureWithSurface("night stand") {

}

class ChestOfDrawers() extends GenericOpenableFurniture("chest of drawers") {

}

class Desk() extends GenericUnmovableFurnitureWithSurface("desk") {

}

class DressingTable() extends GenericUnmovableFurnitureWithSurface("dressing table") {

}

// Bathroom
class Toilet() extends FastObject("toilet") {

}

class Shower() extends GenericUnmovableFurnitureWithSurface("shower") {

}

class Bathtub() extends GenericUnmovableFurnitureWithSurface("bath tub") {

}

class BathroomCabinet() extends GenericOpenableFurniture("bathroom cabinet") {

}

class TowelRack() extends GenericUnmovableFurnitureWithSurface("towel rack") {

}

class WallHook() extends GenericUnmovableFurnitureWithSurface("wall hook") {

}

class BathMat() extends FastObject("bath mat") {
  this.isMovable = false

}

class Sink() extends GenericUnmovableFurnitureWithSurface("sink") {

}

class ToiletRollHolder() extends GenericUnmovableFurnitureWithSurface("toilet roll holder") {

}

// Laundry
class WashingMachine() extends GenericOpenableFurniture("washing machine") {

}

class ClothesDrier() extends GenericOpenableFurniture("clothes drier") {

}

class LaundryBasket() extends GenericUnmovableFurnitureWithSurface("laundry basket") {

}

class ClothesLine() extends GenericUnmovableFurnitureWithSurface("clothes line") {

}

// Workshop?
class WorkTable() extends GenericUnmovableFurnitureWithSurface("work table") {

}

class Bench() extends GenericUnmovableFurnitureWithSurface("bench") {

}

class SuspendedShelf() extends GenericUnmovableFurnitureWithSurface("suspended shelf") {

}

class Shelf() extends GenericUnmovableFurnitureWithSurface("shelf") {

}

class PatioChair() extends GenericUnmovableFurnitureWithSurface("patio chair") {

}

class PatioTable() extends GenericUnmovableFurnitureWithSurface("patio table") {

}

class Workbench() extends GenericUnmovableFurnitureWithSurface("workbench") {

}

class Garden() extends GenericUnmovableFurnitureWithSurface("garden") {
  this.prepositionReferent = "in "
}

// Supermarket
class Showcase() extends GenericUnmovableFurnitureWithSurface("showcase") {
  this.prepositionReferent = "in "
}


// Patio


/*
 * Meal
 */
class Meal() extends FastObject("meal") {
  this.isEdible = true
  this.isMovable = true
}

/*
 * Coin Collector
 */
class Coin() extends FastObject("coin") {
  this.isMovable = true
}

/*
 * Arithmetic Game
 */
class BundleOfObjects(name:String) extends FastObject(name) {
  this.isMovable = true

  override def getDescription():String = {
    return this.name
  }

}


/*
 * Instances
 */

/*
class Food(name:String) extends FastObject(name) {

  override def getDescription(): String = {
    return "a " + this.getFoodAdjectives() + this.name
  }
}

class Apple extends FastObject(name = "green apple") {
  this.isEdible = true
  this.isMovable = true

}

class OrangeJuice extends FastObject(name = "orange juice") {
  this.isEdible = true
  this.isMovable = true

}

class Flour extends FastObject(name = "flour") {
  this.isEdible = true
  this.isMovable = true

  override def getDescription(): String = {
    return "some " + this.getFoodAdjectives() + this.name
  }
}

*/
