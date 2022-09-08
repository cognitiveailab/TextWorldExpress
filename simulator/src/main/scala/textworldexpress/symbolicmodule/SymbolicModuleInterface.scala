package textworldexpress.symbolicmodule

import scala.collection.mutable.ArrayBuffer

class SymbolicModuleInterface {

  val modules = new ArrayBuffer[SymbolicModule]

  /*
   * Adding/enabling modules
   */
  def addModule(moduleName:String): Unit = {
    val moduleNameSanitized = moduleName.trim().toLowerCase

    if (moduleNameSanitized == "calc") {
      println("Adding calc module")
      // TODO
//      this.modules.append( new ModuleCalc() )
    }

  }


  // Get a list of enabled modules
  def getEnabledModuleNames(): Unit = {
    val names = this.modules.map(_.moduleName).sorted
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
    val outSanitized = out.toSet.toArray.sorted
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
