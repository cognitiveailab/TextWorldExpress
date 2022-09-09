package textworldexpress.generator

import textworldexpress.games.{ArithmeticGameGenerator, CoinGameGenerator, KitchenGameGenerator, MapReaderGameGenerator, MapReaderRandomGameGenerator, SimonSaysGameGenerator, SimonSaysMemoryGameGenerator, SortingGameGenerator, TWCGameGenerator, TakeThisActionGameGenerator}
import textworldexpress.goldagent.ArithmeticGoldAgentWithModule
import textworldexpress.runtime.PythonInterface
import textworldexpress.struct.TextGame

import scala.util.Random

/*
 * Abstract class for game generators
 */
abstract class GameGenerator {
  // A string, describing any configuration issues.  If empty, the game is considered valid.
  var errorStr:String = ""

  // Returns true if the game configuration is not valid.
  def isInvalid():Boolean

  // Returns a string describing the game configuration
  def getConfigStr():String

  // Makes a game
  def mkGame(seed:Long, fold:String):TextGame

  // Makes a game (including generating a gold path)
  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String])

  def mkGoldPathModules(r:Random, interface:PythonInterface):(Boolean, Array[String]) = (false, Array.empty[String])

}


/*
 * Kitchen
 */
class GameGeneratorKitchen(val numLocations:Int=11, val numDistractorItems:Int = 10, val numIngredients:Int=3, val includeDoors:Boolean=true, val limitInventorySize:Boolean=true) extends GameGenerator {
  val generator = new KitchenGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    if (numLocations < 1) os.append("Number of locations must be greater than one (specified value = " + numLocations + "). ")
    if (numLocations > 11) os.append("Number of locations must be less than or equal to 11 (specified value = " + numLocations + "). ")

    if (numDistractorItems < 0) os.append("Number of distractor objects must be greater than or equal to zero (specified value = " + numDistractorItems + "). ")
    if (numDistractorItems > 10) os.append("Number of distractor objects must be less than or equal to 10 (specified value = " + numDistractorItems + "). ")

    if (numIngredients < 1) os.append("Number of ingredients must be at least 1 (specified value = " + numIngredients + "). ")
    if (numIngredients > 5) os.append("Number of ingredients must less than 5 (specified value = " + numIngredients + "). ")

    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Kitchen\n")
    os.append("numLocations: " + numLocations + "\n")
    os.append("numDistractorItems: " + numDistractorItems + "\n")
    os.append("numIngredients: " + numIngredients + "\n")
    os.append("includeDoors: " + includeDoors + "\n")
    os.append("limitInventorySize: " + limitInventorySize + "\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, numLocations=numLocations, numDistractorItems=numDistractorItems, numIngredients=numIngredients, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, numLocations=numLocations, numDistractorItems=numDistractorItems, numIngredients=numIngredients, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

}


/*
 * TWC
 */
class GameGeneratorTWC(numLocations:Int = 3, numItemsToPutAway:Int = 4, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {
  val generator = new TWCGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    if (numLocations < 1) os.append("Number of locations must be greater than one (specified value = " + numLocations + "). ")
    if (numLocations > 3) os.append("Number of locations must be less than 3 (specified value = " + numLocations + "). ")

    if (numItemsToPutAway < 0) os.append("Number of items to put away must be at least 1 (specified value = " + numItemsToPutAway + "). ")
    if (numItemsToPutAway > 4) os.append("Number of items to put away must be 4 or fewer (specified value = " + numItemsToPutAway + "). ")

    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: TWC\n")
    os.append("numLocations: " + numLocations + "\n")
    os.append("numItemsToPutAway: " + numItemsToPutAway + "\n")
    os.append("includeDoors: " + includeDoors + "\n")
    os.append("limitInventorySize: " + limitInventorySize + "\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, numLocations=numLocations, numItemsToPutAway=numItemsToPutAway, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, numLocations=numLocations, numItemsToPutAway=numItemsToPutAway, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

}


/*
 * Coin collector
 */
class GameGeneratorCoin(numLocations:Int = 11, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {
  val generator = new CoinGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    if (numLocations < 1) os.append("Number of locations must be greater than one (specified value = " + numLocations + "). ")
    if (numLocations > 11) os.append("Number of locations must be less than or equal to 11 (specified value = " + numLocations + "). ")

    if (numDistractorItems < 0) os.append("Number of distractor objects must be greater than or equal to zero (specified value = " + numDistractorItems + "). ")
    if (numDistractorItems > 10) os.append("Number of distractor objects must be less than or equal to 10 (specified value = " + numDistractorItems + "). ")

    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Coin Collector\n")
    os.append("numLocations: " + numLocations + "\n")
    os.append("numDistractorItems: " + numDistractorItems + "\n")
    os.append("includeDoors: " + includeDoors + "\n")
    os.append("limitInventorySize: " + limitInventorySize + "\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, numLocations=numLocations, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, numLocations=numLocations, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

}


/*
 * Map Reader
 */
class GameGeneratorMapReader(numLocations:Int = 11, maxDistanceApart:Int = 3, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {
  val generator = new MapReaderGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    if (numLocations < 1) os.append("Number of locations must be greater than one (specified value = " + numLocations + "). ")
    if (numLocations > 15) os.append("Number of locations must be less than or equal to 15 (specified value = " + numLocations + "). ")

    if (maxDistanceApart < 1) os.append("Maximum distance apart must be at least 1 (specified value = " + maxDistanceApart + ").")
    if (maxDistanceApart > 4) os.append("Maximum distance apart must be less than or equal to 4 (specified value = " + maxDistanceApart + ").")

    if (numDistractorItems < 0) os.append("Number of distractor objects must be greater than or equal to zero (specified value = " + numDistractorItems + "). ")
    if (numDistractorItems > 10) os.append("Number of distractor objects must be less than or equal to 10 (specified value = " + numDistractorItems + "). ")

    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Map Reader\n")
    os.append("numLocations: " + numLocations + "\n")
    os.append("numDistractorItems: " + numDistractorItems + "\n")
    os.append("includeDoors: " + includeDoors + "\n")
    os.append("limitInventorySize: " + limitInventorySize + "\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

}


/*
 * Map Reader (Random)
 */
class GameGeneratorMapReaderRandom(numLocations:Int = 11, maxDistanceApart:Int = 3, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {
  val generator = new MapReaderRandomGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    if (numLocations < 1) os.append("Number of locations must be greater than one (specified value = " + numLocations + "). ")
    if (numLocations > 50) os.append("Number of locations must be less than or equal to 50 (specified value = " + numLocations + "). ")

    if (maxDistanceApart < 1) os.append("Maximum distance apart must be at least 1 (specified value = " + maxDistanceApart + ").")
    if (maxDistanceApart > 8) os.append("Maximum distance apart must be less than or equal to 4 (specified value = " + maxDistanceApart + ").")

    if (numDistractorItems < 0) os.append("Number of distractor objects must be greater than or equal to zero (specified value = " + numDistractorItems + "). ")
    if (numDistractorItems > 10) os.append("Number of distractor objects must be less than or equal to 10 (specified value = " + numDistractorItems + "). ")

    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Map Reader\n")
    os.append("numLocations: " + numLocations + "\n")
    os.append("numDistractorItems: " + numDistractorItems + "\n")
    os.append("includeDoors: " + includeDoors + "\n")
    os.append("limitInventorySize: " + limitInventorySize + "\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize, fold=fold)
  }

}


/*
 * Arithmetic Game
 */
class GameGeneratorArithmetic() extends GameGenerator {
  val generator = new ArithmeticGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Arithmetic\n")
    os.append("This game has no parameters other than seed, and game fold (train/dev/test).\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, fold=fold)
  }

  override def mkGoldPathModules(r:Random, interface:PythonInterface):(Boolean, Array[String]) = {
    val agent = new ArithmeticGoldAgentWithModule(interface)
    val (success, path) = agent.mkGoldPath(r)
    return (success, path)
  }

}


/*
 * Take This Action Game
 */
class GameGeneratorTakeThisAction() extends GameGenerator {
  val generator = new TakeThisActionGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Take This Action\n")
    os.append("This game has no parameters other than seed, and game fold (train/dev/test).\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, fold=fold)
  }

}


/*
 * Simon Says Game
 */
class GameGeneratorSimonSays() extends GameGenerator {
  val generator = new SimonSaysGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Simon Says\n")
    os.append("This game has no parameters other than seed, and game fold (train/dev/test).\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, fold=fold)
  }

}


/*
 * Simon Says (long memory version) Game
 */
class GameGeneratorSimonSaysMemory() extends GameGenerator {
  val generator = new SimonSaysMemoryGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Simon Says (memory)\n")
    os.append("This game has no parameters other than seed, and game fold (train/dev/test).\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, fold=fold)
  }

}

/*
 * Sorting Game
 */
class GameGeneratorSorting() extends GameGenerator {
  val generator = new SortingGameGenerator()
  this.errorStr = this.checkValidConfiguration()

  /*
   * Error checking
   */
  private def checkValidConfiguration():String = {
    val os = new StringBuilder
    return os.toString()
  }

  def isInvalid():Boolean = {
    if (errorStr.length > 0) return true
    // Otherwise
    return false
  }

  def getConfigStr():String = {
    val os = new StringBuilder()
    os.append("Game: Sorting\n")
    os.append("This game has no parameters other than seed, and game fold (train/dev/test).\n")
    return os.toString()
  }

  /*
   * Game generation
   */
  def mkGame(seed:Long, fold:String):TextGame = {
    return generator.mkGame(seed=seed, fold=fold)
  }

  def mkGameWithGoldPath(seed:Long, fold:String):(TextGame, Array[String]) = {
    return generator.mkGameWithGoldPath(seed=seed, fold=fold)
  }

}


/*
 * Generic generator
 */
object GameGenerator {


  // Make the kitchen game
  private def mkKitchen(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array("numLocations", "numDistractorItems", "numIngredients", "includeDoors", "limitInventorySize")

    val numLocations:Int            = properties.getOrElse("numLocations", 11)
    val numDistractorItems:Int      = properties.getOrElse("numDistractorItems", 10)
    val numIngredients:Int          = properties.getOrElse("numIngredients", 3)
    val includeDoors:Boolean        = if(properties.getOrElse("includeDoors", 1) == 1) { true } else { false }
    val limitInventorySize:Boolean  = if(properties.getOrElse("limitInventorySize", 1) == 1) { true } else { false }

    // Make game
    val game = new GameGeneratorKitchen(numLocations=numLocations, numDistractorItems=numDistractorItems, numIngredients=numIngredients, includeDoors=includeDoors, limitInventorySize=limitInventorySize)

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + ". ")
    }

    return game
  }

  // Make the TWC game
  private def mkTWC(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array("numLocations", "numItemsToPutAway", "includeDoors", "limitInventorySize")

    val numLocations:Int            = properties.getOrElse("numLocations", 3)
    val numItemsToPutAway:Int      = properties.getOrElse("numItemsToPutAway", 4)
    val includeDoors:Boolean        = if(properties.getOrElse("includeDoors", 0) == 1) { true } else { false }
    val limitInventorySize:Boolean  = if(properties.getOrElse("limitInventorySize", 0) == 1) { true } else { false }

    // Make game
    val game = new GameGeneratorTWC(numLocations=numLocations, numItemsToPutAway=numItemsToPutAway, includeDoors=includeDoors, limitInventorySize=limitInventorySize)

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + ". ")
    }

    return game
  }


  // Make the coin game
  private def mkCoin(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array("numLocations", "numDistractorItems", "includeDoors", "limitInventorySize")
    // class GameGeneratorCoin(numLocations:Int = 11, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {

    val numLocations:Int            = properties.getOrElse("numLocations", 11)
    val numDistractorItems:Int      = properties.getOrElse("numDistractorItems", 0)
    val includeDoors:Boolean        = if(properties.getOrElse("includeDoors", 1) == 1) { true } else { false }
    val limitInventorySize:Boolean  = if(properties.getOrElse("limitInventorySize", 1) == 1) { true } else { false }

    // Make game
    val game = new GameGeneratorCoin(numLocations=numLocations, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize)

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + ". ")
    }

    return game
  }

  // Make the map reader game
  private def mkMapReader(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array("numLocations", "numDistractorItems", "includeDoors", "limitInventorySize", "maxDistanceApart")
    // class GameGeneratorCoin(numLocations:Int = 11, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {

    val numLocations:Int            = properties.getOrElse("numLocations", 15)
    val maxDistanceApart:Int        = properties.getOrElse("maxDistanceApart", 4)
    val numDistractorItems:Int      = properties.getOrElse("numDistractorItems", 0)
    val includeDoors:Boolean        = if(properties.getOrElse("includeDoors", 0) == 1) { true } else { false }
    val limitInventorySize:Boolean  = if(properties.getOrElse("limitInventorySize", 0) == 1) { true } else { false }

    // Make game
    val game = new GameGeneratorMapReader(numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize)

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + ". ")
    }

    return game
  }

  // Make the map reader (random map) game
  private def mkMapReaderRandom(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array("numLocations", "numDistractorItems", "includeDoors", "limitInventorySize", "maxDistanceApart")
    // class GameGeneratorCoin(numLocations:Int = 11, numDistractorItems:Int = 0, includeDoors:Boolean = false, limitInventorySize:Boolean = false) extends GameGenerator {

    val numLocations:Int            = properties.getOrElse("numLocations", 15)
    val maxDistanceApart:Int        = properties.getOrElse("maxDistanceApart", 4)
    val numDistractorItems:Int      = properties.getOrElse("numDistractorItems", 0)
    val includeDoors:Boolean        = if(properties.getOrElse("includeDoors", 0) == 1) { true } else { false }
    val limitInventorySize:Boolean  = if(properties.getOrElse("limitInventorySize", 0) == 1) { true } else { false }

    // Make game
    val game = new GameGeneratorMapReaderRandom(numLocations=numLocations, maxDistanceApart=maxDistanceApart, numDistractorItems=numDistractorItems, includeDoors=includeDoors, limitInventorySize=limitInventorySize)

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + ". ")
    }

    return game
  }

  // Make the arithmetic game
  private def mkArithmetic(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array()

    // Make game
    val game = new GameGeneratorArithmetic()

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + "None. ")
    }

    return game
  }

  // Make the 'take this action' game
  private def mkTakeThisAction(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array()

    // Make game
    val game = new GameGeneratorTakeThisAction()

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + "None. ")
    }

    return game
  }

  // Make the 'simon says' game
  private def mkSimonSays(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array()

    // Make game
    val game = new GameGeneratorSimonSays()

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + "None. ")
    }

    return game
  }

  // Make the 'simon says' game
  private def mkSimonSaysMemory(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array()

    // Make game
    val game = new GameGeneratorSimonSaysMemory()

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + "None. ")
    }

    return game
  }

  // Make the 'sorting' game
  private def mkSorting(properties:Map[String, Int]):GameGenerator = {
    val knownPropertyNames          = Array()

    // Make game
    val game = new GameGeneratorSorting()

    // Check for unrecognized properties
    for (propName <- properties.keySet) {
      if (!knownPropertyNames.contains(propName)) game.errorStr += ("Unrecognized property name (" + propName + ").  Known properties: " + knownPropertyNames.mkString(", ") + "None. ")
    }

    return game
  }


  /*
   * The main generator.
   * Returns (success, GameGenerator)
   */
  val VALID_GAME_NAMES = Array("cookingworld", "twc", "coin", "mapreader", "mapreader-random", "arithmetic", "takethisaction", "simonsays", "simonsays-memory", "sorting")

  def mkGameGenerator(gameName:String, properties:Map[String, Int] = Map[String, Int]()):(Boolean, GameGenerator) = {

    gameName.toLowerCase match {
      case "cookingworld" => {
        val game = this.mkKitchen(properties)
        return (!game.isInvalid(), game)
      }
      case "twc" => {
        val game = this.mkTWC(properties)
        return (!game.isInvalid(), game)
      }
      case "coin" => {
        val game = this.mkCoin(properties)
        return (!game.isInvalid(), game)
      }
      case "mapreader" => {
        val game = this.mkMapReader(properties)
        return (!game.isInvalid(), game)
      }
      case "mapreader-random" => {
        val game = this.mkMapReaderRandom(properties)
        return (!game.isInvalid(), game)
      }
      case "arithmetic" => {
        val game = this.mkArithmetic(properties)
        return (!game.isInvalid(), game)
      }
      case "takethisaction" => {
        val game = this.mkTakeThisAction(properties)
        return (!game.isInvalid(), game)
      }
      case "simonsays" => {
        val game = this.mkSimonSays(properties)
        return (!game.isInvalid(), game)
      }
      case "simonsays-memory" => {
        val game = this.mkSimonSaysMemory(properties)
        return (!game.isInvalid(), game)
      }
      case "sorting" => {
        val game = this.mkSorting(properties)
        return (!game.isInvalid(), game)
      }
      // Unknown case: Game not recognized.
      case _ => {
        val game = this.mkCoin( Map[String, Int]() )    // Placeholder game, with empty properties
        game.errorStr += ("ERROR: Unknown game (" + gameName + ").  Valid game names: " + VALID_GAME_NAMES.mkString(", ") + ". ")
        return (false, game)
      }
    }

  }

}
