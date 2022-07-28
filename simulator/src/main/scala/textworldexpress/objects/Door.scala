package textworldexpress.objects

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


// Special case of FastObject -- only used through room description, never exposed directly to the environment
case class Door(val doorDescription:String, var _isOpen:Boolean) extends FastObject("door") {
  this.isOpen = _isOpen

  // Attempt at a deep copy
  def deepCopy():Door = {
    // Door never contains anything or has any other properties, so cloning is cheap.
    val out = new Door(this.doorDescription, this.isOpen)

    // Return
    out
  }

  override def getDescription(): String = {
    return this.doorDescription
  }

}


class DoorMaker() {
  val lut = mutable.Map[String, Array[String]]()

  // Constructor
  this.init()

  private def mkKey(location1:String, location2:String):String = {
    val key = location1 + "+" + location2
    return key
  }

  private def addToLUT(location1:String, location2:String, value:Array[String]): Unit = {
    val key = this.mkKey(location1, location2)
    lut(key) = value
  }

  private def init(): Unit = {
    this.addToLUT("pantry", "kitchen", Array("frosted-glass door", "plain door") )
    this.addToLUT("kitchen", "backyard", Array("sliding patio door", "patio door", "screen door") )
    this.addToLUT("corridor", "backyard", Array("sliding patio door", "patio door", "screen door") )
    this.addToLUT("living room", "backyard", Array("sliding patio door", "patio door", "screen door") )
    this.addToLUT("living room", "driveway", Array("front door", "fiberglass door") )
    this.addToLUT("corridor", "driveway", Array("front door", "fiberglass door") )
    this.addToLUT("supermarket", "street", Array("sliding door", "commercial glass door") )

    val genericDoors = Array("wood door")
    this.addToLUT("bedroom", "living room", genericDoors )
    this.addToLUT("bedroom", "bathroom", genericDoors )
    this.addToLUT("bedroom", "corridor", genericDoors )
    this.addToLUT("bathroom", "living room", genericDoors )
    this.addToLUT("bathroom", "bathroom", genericDoors )
    this.addToLUT("bathroom", "corridor", genericDoors )
    this.addToLUT("bathroom", "kitchen", genericDoors )
    this.addToLUT("laundry room", "kitchen", genericDoors )
    this.addToLUT("laundry room", "bathroom", genericDoors )
    this.addToLUT("laundry room", "corridor", genericDoors )
  }


  // Generator
  // Make a random door
  def mkDoor(r:Random, location1:String, location2:String, isOpen:Boolean):Option[Door] = {
    val key = this.mkKey(location1, location2)
    if (!this.lut.contains(key)) return None
    val possibleDoors = this.lut(key)
    val randIdx = r.nextInt(possibleDoors.length)
    val description = possibleDoors(randIdx)
    val door = new Door(doorDescription = description, isOpen)
    return Some(door)
  }

}