package textworldexpress.symbolicmodule

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks._
import ModuleKnowledgeBaseTWC.ACTION_PREFIX

class ModuleKnowledgeBaseTWC(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {
  // Load knowledge base
  val knowledgeBaseRows = ModuleKnowledgeBaseTWC.loadTSV(ModuleKnowledgeBaseTWC.KB_FILENAME)

  // Precached actions
  val precachedValidActions = this.mkValidActions()


  override def getValidCommands(): Array[String] = {
    return this.precachedValidActions
  }

  override def runCommand(actionStr: String): String = {
    // Step 1: Make sure this is a valid command
    if (!this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Step 2: Get query results.
    val queryStr = actionStr.substring(ACTION_PREFIX.length).trim()
    val queryResults = this.runQuery(queryStr)

    // Step 3: Check that we have a non-zero number of results
    if (queryResults.length == 0) {
      return "No results found."
    }

    // Step 4: Return the results
    val os = new StringBuilder()
    os.append("The results are:\n ")
    for (result <- queryResults) {
      os.append(result.mkString(" ") + ".\n ")
    }

    // Return
    return os.toString()
  }

  /*
   * Precachine
   */
  private def mkValidActions():Array[String] = {
    val out = new ArrayBuffer[String]

    for (row <- this.knowledgeBaseRows) {
      out.append(ACTION_PREFIX + row(0)) // First element of the triple
      out.append(ACTION_PREFIX + row(1)) // Second element of the triple
      out.append(ACTION_PREFIX + row(2)) // Third element of the triple
    }

    //## println(out.mkString(", "))

    // Convert to a set to remove duplicates
    return out.toSet.toArray.sorted
  }

  /*
   * Run queries
   */
  def runQuery(queryStr:String): Array[Array[String]] = {
    val out = new ArrayBuffer[Array[String]]
    val queryStrSanitized = queryStr.trim().toLowerCase

    // For each row in the knowledge base
    for (row <- this.knowledgeBaseRows) {
      breakable {
        // Check to see if any one of the columns contains the query text.  If it does, include it in the results.
        for (col <- row) {
          if (col.toLowerCase == queryStrSanitized) {
            out.append(row)
            break()
          }
        }
      }
    }

    // Return
    out.toArray
  }


}


object ModuleKnowledgeBaseTWC {
  val MODULE_NAME   = "kb-twc"
  val KB_FILENAME   = "kb-twc.tsv"

  val ACTION_PREFIX = "query "

  // Load a TSV file
  def loadTSV(filenameIn:String): Array[Array[String]] = {
    val out = new ArrayBuffer[Array[String]]
    for (line <- Source.fromFile(filenameIn).getLines) {
      if (line.trim().length > 1) {
        val fields = line.split("\t").map(_.trim())
        out.append(fields)
      }
    }
    out.toArray
  }


}