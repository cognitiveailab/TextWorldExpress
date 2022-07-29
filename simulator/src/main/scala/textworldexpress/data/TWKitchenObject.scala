package textworldexpress.data

import textworldexpress.objects.FastObject

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

case class TWKitchenObject(name:String, indefinite:String, properties:Set[String], locations:Array[String]) {

  // Convert to a FastObject
  def toFastObject():FastObject = {
    val out = new FastObject(name=name)
    out.canonicalLocations = locations      // Store it's canonical locations
    out.isMovable = true                    // Make the object movable
    if (indefinite.length > 0) out.indefinite = indefinite


    if (properties.contains("edible")) out.isEdible = true
    if (properties.contains("inedible")) out.isEdible = false
    if (properties.contains("cookable")) out.isCookable = true
    if (properties.contains("needs_cooking")) out.needsCooking = true
    if (properties.contains("drinkable")) out.isDrinkable = true
    if (properties.contains("cuttable")) out.isCuttable = true
    if (properties.contains("raw")) out.isRaw = true
    if (properties.contains("uncut")) out.isCut = false

    // TODO: Properties
    // edible, inedible,
    // cookable, needs_cooking,
    // drinkable,
    // cuttable, uncut
    // raw


    return out
  }

  override def toString():String = {
    return "name: " + name + ", indefinite: " + indefinite + ", properties: " + properties.mkString(", ") + ", locations: " + locations.mkString(", ")
  }

}


// Storage class for one ingredient
case class RecipeIngredient(name:String, preparation:Set[String]) {
  val requiresPrep = this.requiresPreparation()

  private def requiresPreparation():Boolean = {
    if (preparation.contains("raw") && preparation.contains("uncut")) return false
    // Otherwise
    return true
  }

  override def toString():String = {
    return "ingredient: " + name + ", preparation: " + preparation.mkString(", ")
  }

}

// Loads the data from TWC Kitchen
class LoadTWKitchenDataJSON(filename:String = LoadTWKitchenDataJSON.DEFAULT_FILENAME) {

  val (allObjs, lutObj, lutLocation, foodSplitsTrain, foodSplitsDev, foodSplitsTest, foodPrepTrain, foodPrepDev, foodPrepTest) = this.load(filename)

  /*
   * Getters
   */
  def getObjByName(name:String):TWKitchenObject = {
    return lutObj(name)
  }

  def mkFastObjectByName(name:String):FastObject = {
    return lutObj(name).toFastObject()
    /*
    if (lutObj.contains(name)) return lutObj(name).toFastObject()
    // Some objects don't exist, so make a default one
    // TODO: SLOW?
    val obj = new TWKitchenObject(name, indefinite = "", properties = Set("raw", "uncut", "cuttable", "cookable", "edible"), locations = Array("counter", "shelf", "kitchen cupboard"))
    return obj.toFastObject()
     */
  }

  def mkRandomObjectByLocation(r:Random, location:String):Option[FastObject] = {
    if (!this.lutLocation.contains(location)) return None

    val validObjects = this.lutLocation(location)
    if (validObjects.isEmpty) return None

    val randIdx = r.nextInt(validObjects.length)
    val obj = validObjects(randIdx).toFastObject()
    return Some(obj)
  }


  // Fold: "train", "dev", or "test"
  def mkRandomRecipe(r:Random, numIngredients:Int, fold:String): ArrayBuffer[RecipeIngredient] = {
    val validFoodPreps = if (fold == "train") { foodPrepTrain } else if (fold == "dev") { foodPrepDev } else if (fold == "test") { foodPrepTest } else { throw new RuntimeException("ERROR: Unknown fold (" + fold + "). Should be 'train', 'dev, 'or 'test'.") }
    val ingredientKeys = validFoodPreps.keySet.toArray

    // Step 1: Pick N random ingredients
    val randIngredientIdxs = new ArrayBuffer[Int]()
    var attempts:Int = 0
    while ((randIngredientIdxs.length < numIngredients) && (attempts < 100)) {
      val randIdx = r.nextInt(ingredientKeys.size)
      if (!randIngredientIdxs.contains(randIdx)) {
        randIngredientIdxs.append(randIdx)
      }
      attempts += 1
    }

    val ingredients = new ArrayBuffer[RecipeIngredient]()
    for (idx <- randIngredientIdxs) {
      val name = ingredientKeys(idx)
      val possiblePreps = validFoodPreps(name)
      val randIdx = r.nextInt(possiblePreps.length)
      val randPrep = possiblePreps(randIdx)

      val recipeIngredient = new RecipeIngredient(name, randPrep)
      ingredients.append(recipeIngredient)

    }

    return ingredients
  }


  /*
   * Loading/initialization
   */

  // Load the JSON file, and convert to storage classes referenced by look-up-tables.
  def load(filenameIn:String):(Array[TWKitchenObject], Map[String, TWKitchenObject], Map[String, ArrayBuffer[TWKitchenObject]], Set[String], Set[String], Set[String], Map[String, Array[Set[String]]], Map[String, Array[Set[String]]], Map[String, Array[Set[String]]]) = {
    // Step 1: Load the TextWorld Common Sense object file
    val jsonString = Source.fromFile(filenameIn).getLines.mkString
    val dataRaw = ujson.read(jsonString).value.asInstanceOf[mutable.LinkedHashMap[String, Any]]

    // FRESH_ADJECTIVES
    // ROTTEN_ADJECTIVES
    // TYPES_OF_COOKING
    // TYPES_OF_CUTTING
    // TYPES_OF_COOKING_VERBS
    // TYPES_OF_CUTTING_VERBS
    // FOOD_SPLITS
    // FOOD_PREPARATIONS_SPLITS
    // FOODS_COMPACT

    val foodsCompact = dataRaw("FOODS_COMPACT").asInstanceOf[ujson.Obj].value.asInstanceOf[mutable.LinkedHashMap[String, Any]]
    //println ("foodsCompact: " + foodsCompact)

    // Step 2A: Objects (foods_compact): Convert from the JSON format to an internal storage class (TWCObject)
    val out = new ArrayBuffer[TWKitchenObject]()
    for (key <- foodsCompact.keySet) {
      val record = foodsCompact(key).asInstanceOf[ujson.Obj].value
      //println("key: " + key + "   " + record.toString())

      val name = key
      var indefinite = ""
      if (record.contains("indefinite")) indefinite = record("indefinite").asInstanceOf[ujson.Str].toString().replaceAll("\"", "")
      val properties = record("properties").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet
      val locations = record("locations").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "").split("\\.").last).toArray

      var alternateNames: Array[String] = Array.empty[String]
      if (record.contains("names")) {
        alternateNames = record("names").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "").split("\\.").last).toArray
      }


      if (alternateNames.length == 0) {
        // Single name
        out.append( new TWKitchenObject(name, indefinite, properties, locations) )
      } else {
        // This object has multiple names
        for (altName <- alternateNames) {
          out.append(new TWKitchenObject(altName, indefinite, properties, locations))
        }
      }
      //println("obj: " + obj.toString())
    }

    /*
    for (i <- 0 until out.length) {
      println(i + ": " + out(i).toString())
    }
     */

    // Step 2A: Make object look-up-table
    val lutObj = mutable.Map[String, TWKitchenObject]()
    for (obj <- out) {
      lutObj(obj.name) = obj
    }

    // Step 2B: Make location look-up-table
    val lutLocation = mutable.Map[String, ArrayBuffer[TWKitchenObject]]()
    for (obj <- out) {
      for (location <- obj.locations) {
        if (!lutLocation.contains(location)) lutLocation(location) = new ArrayBuffer[TWKitchenObject]()
        lutLocation(location).append(obj)
      }
    }

    //println("lutLocation keys: " + lutLocation.keySet.mkString(", "))

    // Step 3: Food splits (train/dev/test): Food names
    val foodSplits = dataRaw("FOODS_SPLITS").asInstanceOf[ujson.Obj].value.asInstanceOf[mutable.LinkedHashMap[String, Any]]
    val foodSplitsTrain = foodSplits("train").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet
    val foodSplitsDev = foodSplits("valid").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet
    val foodSplitsTest = foodSplits("test").asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet

    /*
    println ("foodsSplits: " + foodSplits)
    println ("foodSplitsTrain: " + foodSplitsTrain)
    println ("foodSplitsDev: " + foodSplitsDev)
    println ("foodSplitsTest: " + foodSplitsTest)
     */

    // Step 4: Food preparation splits (train/dev/test): Foods and how they need to be prepared
    val foodPreparationsSplits = dataRaw("FOOD_PREPARATIONS_SPLITS").asInstanceOf[ujson.Obj].value.asInstanceOf[mutable.LinkedHashMap[String, Any]]

    // Step 4A: train
    val foodPrepsTrainRaw = foodPreparationsSplits("train").asInstanceOf[ujson.Obj].value //.map(_.toString().replaceAll("\"", ""))
    val foodPrepsTrain = mutable.Map[String, Array[Set[String]]]()
    for (elem <- foodPrepsTrainRaw) {
      val preps = new ArrayBuffer[ Set[String] ]
      val foodName = elem._1
      val prepList = elem._2.asInstanceOf[ujson.Arr].value
      for (onePrep <- prepList) {
        preps.append( onePrep.asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet )
      }
      foodPrepsTrain(foodName) = preps.toArray
    }

    // Step 4B: dev
    val foodPrepsDevRaw = foodPreparationsSplits("valid").asInstanceOf[ujson.Obj].value //.map(_.toString().replaceAll("\"", ""))
    val foodPrepsDev = mutable.Map[String, Array[Set[String]]]()
    for (elem <- foodPrepsDevRaw) {
      val preps = new ArrayBuffer[ Set[String] ]
      val foodName = elem._1
      val prepList = elem._2.asInstanceOf[ujson.Arr].value
      for (onePrep <- prepList) {
        preps.append( onePrep.asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet )
      }
      foodPrepsDev(foodName) = preps.toArray
    }

    // Step 4C: test
    val foodPrepsTestRaw = foodPreparationsSplits("test").asInstanceOf[ujson.Obj].value //.map(_.toString().replaceAll("\"", ""))
    val foodPrepsTest = mutable.Map[String, Array[Set[String]]]()
    for (elem <- foodPrepsTestRaw) {
      val preps = new ArrayBuffer[ Set[String] ]
      val foodName = elem._1
      val prepList = elem._2.asInstanceOf[ujson.Arr].value
      for (onePrep <- prepList) {
        preps.append( onePrep.asInstanceOf[ujson.Arr].value.map(_.toString().replaceAll("\"", "")).toSet )
      }
      foodPrepsTest(foodName) = preps.toArray
    }

    // Return
    return (out.toArray, lutObj.toMap, lutLocation.toMap, foodSplitsTrain, foodSplitsDev, foodSplitsTest, foodPrepsTrain.toMap, foodPrepsDev.toMap, foodPrepsTest.toMap)
  }

}


object LoadTWKitchenDataJSON {
  val DEFAULT_FILENAME = "cooking_world.json"

  def main(args:Array[String]): Unit = {
    val d = new LoadTWKitchenDataJSON()
    println ("Loaded " + d.allObjs.length + " objects")
  }

}