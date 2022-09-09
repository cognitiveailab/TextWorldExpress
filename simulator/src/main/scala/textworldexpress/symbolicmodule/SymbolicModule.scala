package textworldexpress.symbolicmodule

class SymbolicModule(val moduleName:String, properties:Map[String, Int]) {


  /*
   * Action generation/verification
   */
  def getValidCommands():Array[String] = {
    return Array.empty[String]
  }

  def isValidCommand(actionStr:String):Boolean = {
    if (this.getValidCommands().contains(actionStr)) return true
    // Otherwise
    return false
  }


  /*
   * Observation scraping
   */
  def scrapeObservationStr(observationStr:String): Unit = {

  }

  def scrapeInventoryStr(inventoryStr:String): Unit = {

  }

  def scrapeFreeLookStr(freeLookStr:String): Unit = {

  }


  /*
   * Running commands
   */
  def runCommand(actionStr:String): String = {
    // Step 1: Check that this command is valid
    if (this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Valid command
    return ""
  }


}



object SymbolicModule {

  def mkErrorMessageInvalidCommand(moduleName:String, actionStr:String): String = {
    return "This is not a valid command for this module (" + moduleName + ", " + actionStr + ")"
  }
}