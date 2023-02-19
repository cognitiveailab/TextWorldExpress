package textworldexpress.data

import textworldexpress.objects.FastObject

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

// Storage class
case class TWCObject(name:String, locations:Array[String]) {

  // Convert to a FastObject
  def toFastObject():FastObject = {
    val out = new FastObject(name=name)
    out.canonicalLocations = locations      // Store it's canonical locations
    out.isMovable = true                    // Make the object movable
    return out
  }

  override def toString():String = {
    return "name: " + name + ", locations: " + locations.mkString(", ")
  }

}

// Loader
class LoadTWCDataJSON(filename:String = LoadTWCDataJSON.DEFAULT_FILENAME) {

  val (allObjsTrain, lutObjTrain, lutLocationTrain) = this.load(filename, "train")
  val (allObjsDev, lutObjDev, lutLocationDev) = this.load(filename, "valid")
  val (allObjsTest, lutObjTest, lutLocationTest) = this.load(filename, "test")


  /*
   * Getters
   */
  def getObjByName(name:String, fold:String):TWCObject = {
    if (fold == "train")  return lutObjTrain(name)
    if (fold == "dev")    return lutObjDev(name)
    if (fold == "test")   return lutObjTest(name)
    throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
  }

  def mkFastObjectByName(name:String, fold:String):FastObject = {
    if (fold == "train")  return lutObjTrain(name).toFastObject()
    if (fold == "dev")    return lutObjDev(name).toFastObject()
    if (fold == "test")   return lutObjTest(name).toFastObject()
    throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
  }

  def mkRandomObjectByLocation(r:Random, location:String, fold:String):Option[FastObject] = {
    if (fold == "train")  return this.mkRandomObjectByLocation(r, location, lutLocationTrain)
    if (fold == "dev")    return this.mkRandomObjectByLocation(r, location, lutLocationDev)
    if (fold == "test")   return this.mkRandomObjectByLocation(r, location, lutLocationTest)
    throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
  }

  // Make a random object from a specific set (train/dev/test), but do not respect the location constraints.
  def mkRandomObject(r:Random, fold:String):Option[FastObject] = {
    if (!Array("train", "dev", "test").contains(fold)) {
      throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
    }

    val objs = if (fold == "train") { allObjsTrain } else if (fold == "dev") { allObjsDev } else if (fold == "test") { allObjsTest } else { allObjsTrain }
    if (objs.length == 0) return None

    val randObjIdx = r.nextInt(objs.length)
    val randObj = objs(randObjIdx).toFastObject()

    return Some(randObj)
  }

  private def mkRandomObjectByLocation(r:Random, location:String, lutLocation:Map[String, ArrayBuffer[TWCObject]]):Option[FastObject] = {
    if (!lutLocation.contains(location)) return None

    val validObjects = lutLocation(location)
    if (validObjects.isEmpty) return None

    val randIdx = r.nextInt(validObjects.length)
    val obj = validObjects(randIdx).toFastObject()
    return Some(obj)
  }


  /*
   * Loading/initialization
   */

  // Load the JSON file, and convert to storage classes referenced by look-up-tables.
  def load(filenameIn:String, foldName:String):(Array[TWCObject], Map[String, TWCObject], Map[String, ArrayBuffer[TWCObject]]) = {
    // Step 1: Load the TextWorld Common Sense object file
    val jsonString = Source.fromFile(filenameIn).getLines.mkString
    val dataRawAllFolds = ujson.read(jsonString).value.asInstanceOf[mutable.LinkedHashMap[String, Any]]

    // Get specific fold (train, valid, test)
    val dataRaw = dataRawAllFolds(foldName).asInstanceOf[ujson.Obj].value

    // Step 2: Convert from the JSON format to an internal storage class (TWCObject)
    val out = new ArrayBuffer[TWCObject]()
    for (key <- dataRaw.keySet) {
      val record = dataRaw(key).asInstanceOf[ujson.Obj].value
      val name = record("name").toString().replaceAll("\"", "")
      val locations = record("locations").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toArray
      //println(record.toString())
      out.append(new TWCObject(name, locations))
    }

    /*
    for (i <- 0 until out.length) {
      println(i + ": " + out(i).toString())
    }
     */

    // Step 3: Make object look-up-table
    val lutObj = mutable.Map[String, TWCObject]()
    for (obj <- out) {
      lutObj(obj.name) = obj
    }

    // Step 4: Make location look-up-table
    val lutLocation = mutable.Map[String, ArrayBuffer[TWCObject]]()
    for (obj <- out) {
      for (location <- obj.locations) {
        if (!lutLocation.contains(location)) lutLocation(location) = new ArrayBuffer[TWCObject]()
        lutLocation(location).append(obj)
      }
    }

    //println("lutLocation keys: " + lutLocation.keySet.mkString(", "))

    // Return
    return (out.toArray, lutObj.toMap, lutLocation.toMap)
  }

}


object LoadTWCDataJSON {
  val DEFAULT_FILENAME = "twc_objects.folds.json"

  def main(args:Array[String]): Unit = {
    val d = new LoadTWCDataJSON()
    val allObjs = d.allObjsTrain ++ d.allObjsDev ++ d.allObjsTest
    println ("Loaded " + allObjs.length + " objects")
  }

}
