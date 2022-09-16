package textworldexpress.symbolicmodule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SymbolicModuleInterface(val properties:Map[String, Int]) {

  val modules = new ArrayBuffer[SymbolicModule]

  /*
   * Adding/enabling modules
   */
  def addModule(moduleName:String): Unit = {
    val moduleNameSanitized = moduleName.trim().toLowerCase

    if (moduleNameSanitized == ModuleCalc.MODULE_NAME) {
      //println("Adding calc module")
      this.modules.append(new ModuleCalc(properties))

    } else if (moduleNameSanitized == ModuleKnowledgeBaseTWC.MODULE_NAME) {
      this.modules.append(new ModuleKnowledgeBaseTWC(properties))

    } else if (moduleNameSanitized == ModuleSortByQuantity.MODULE_NAME) {
      this.modules.append(new ModuleSortByQuantity(properties))

    } else if (moduleNameSanitized == ModuleNavigation.MODULE_NAME) {
      this.modules.append(new ModuleNavigation(properties))

    } else {
      // Default
      println ("Cannot add unknown module (" + moduleNameSanitized + ").")
    }

  }


  // Get a list of enabled modules
  def getEnabledModuleNames(): Array[String] = {
    val names = this.modules.map(_.moduleName).sorted.toArray
    return names
  }


  /*
   * Valid commands: Get a list of valid commands from all modules
   */
  def getValidCommands(): Array[String] = {
    // Step 1: Get commands
    val out = new ArrayBuffer[String]
    for (module <- modules) {
      out.insertAll(out.length, module.getValidCommands())
    }

    // Step 2: Remove any duplicates (just in case), and keep a consistent order
    // NOTE: Removing duplicates is now expected in the modules themselves, for speed/efficiency.
    val outSanitized = out.toArray.sorted
    return outSanitized
  }


  /*
   * Update modules with the current environment status
   */
  def giveEnvironmentStatus(observationStr:String, invStr:String, freelookStr:String): Unit = {
    for (module <- this.modules) {
      module.scrapeObservationStr(observationStr)
      module.scrapeInventoryStr(invStr)
      module.scrapeFreeLookStr(freelookStr)
    }
  }


  /*
   * Running commands
   */
  // Returns (success, module output string)
  def runCommand(actionStr:String): (Boolean, String) = {
    for (module <- this.modules) {
      if (module.isValidCommand(actionStr)) {
        return (true, module.runCommand(actionStr))
      }
    }
    // Default: No module has this as a valid command
    return (false, "")
  }

}



object SymbolicModuleInterface {

  def testCalc(): Unit = {
    val properties = mutable.Map[String, Int]()
    properties("hidden_num1") = 10
    properties("hidden_num2") = 15

    val smi = new SymbolicModuleInterface(properties.toMap)

    smi.addModule(ModuleCalc.MODULE_NAME)

    println ("Enabled modules: " + smi.getEnabledModuleNames().mkString(", "))

    println ("Valid commands: " + smi.getValidCommands().mkString(", "))

    println ("")

    for (validCommand <- smi.getValidCommands()) {
      println ("Command: " + validCommand)
      println ("\t Output: " + smi.runCommand(validCommand))
      println ("")
    }

  }


  def testKBTWC(): Unit = {
    val properties = mutable.Map[String, Int]()

    val smi = new SymbolicModuleInterface(properties.toMap)

    smi.addModule(ModuleKnowledgeBaseTWC.MODULE_NAME)

    println ("Enabled modules: " + smi.getEnabledModuleNames().mkString(", "))

    println ("Valid commands: " + smi.getValidCommands().mkString(", "))

    println ("")

    for (validCommand <- smi.getValidCommands()) {
      println ("Command: " + validCommand)
      println ("\t Output: " + smi.runCommand(validCommand))
      println ("")
    }

  }


  def testNavigation(): Unit = {

    val properties = mutable.Map[String, Int]()
    val smi = new SymbolicModuleInterface(properties.toMap)

    smi.addModule(ModuleNavigation.MODULE_NAME)

    println ("Enabled modules: " + smi.getEnabledModuleNames().mkString(", "))

    val obsStr1 = "You are in the kitchen. To the north is the living room. To the east is the hallway. To the south is the backyard. To the west is the pantry."

    val obsStr2 =
      """The map reads:
        |  The pantry connects to the kitchen.
        |  The backyard connects to the kitchen, corridor, street and sideyard.
        |  The alley connects to the driveway and street.
        |  The sideyard connects to the backyard.
        |  The garage connects to the driveway.
        |  The kitchen connects to the backyard, laundry room, living room and pantry.
        |  The street connects to the supermarket, alley and backyard.
        |  The bathroom connects to the living room.
        |  The driveway connects to the alley and garage.
        |  The supermarket connects to the street.
        |  The foyer connects to the corridor.
        |  The laundry room connects to the kitchen.
        |  The corridor connects to the backyard and foyer.
        |  The living room connects to the bedroom, bathroom and kitchen.
        |  The bedroom connects to the living room.
      """.stripMargin

    smi.giveEnvironmentStatus(obsStr1, "", "")
    smi.giveEnvironmentStatus(obsStr2, "", "")

    println ("Valid commands: " + smi.getValidCommands().mkString(", "))

    println ("")

    for (validCommand <- smi.getValidCommands()) {
      println ("Command: " + validCommand)
      println ("\t Output: " + smi.runCommand(validCommand))
      println ("")
    }


  }

  def main(args:Array[String]): Unit = {

    //testCalc()
    //testKBTWC()

    testNavigation()

  }

}
