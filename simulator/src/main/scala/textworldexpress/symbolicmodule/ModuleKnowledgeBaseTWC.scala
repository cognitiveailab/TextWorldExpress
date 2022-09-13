package textworldexpress.symbolicmodule

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks._
import ModuleKnowledgeBaseTWC.ACTION_PREFIX

import scala.collection.mutable

class ModuleKnowledgeBaseTWC(val properties:Map[String, Int]) extends SymbolicModule(ModuleCalc.MODULE_NAME, properties) {

  override def getValidCommands(): Array[String] = {
    return ModuleKnowledgeBaseTWC.precachedValidActions
  }

  override def runCommand(actionStr: String): String = {
    // Step 1: Make sure this is a valid command
    if (!this.isValidCommand(actionStr)) {
      return SymbolicModule.mkErrorMessageInvalidCommand(this.moduleName, actionStr)
    }

    // Get results from precached query
    return ModuleKnowledgeBaseTWC.precachedResults(actionStr)
  }

}


object ModuleKnowledgeBaseTWC {
  val MODULE_NAME   = "kb-twc"
  val KB_FILENAME   = "kb-twc.tsv"

  val ACTION_PREFIX = "query "

  // Load the KB
  val knowledgeBaseRows = ModuleKnowledgeBaseTWC.loadTSV(ModuleKnowledgeBaseTWC.KB_FILENAME)

  // Precached actions
  val precachedValidActions = this.mkValidActions()
  val precachedResults = this.mkPrecachedQueries()

  /*
   * Precaching
   */
  private def mkValidActions():Array[String] = {
    val out = new ArrayBuffer[String]

    for (row <- ModuleKnowledgeBaseTWC.knowledgeBaseRows) {
      out.append(ACTION_PREFIX + row(0)) // First element of the triple
      out.append(ACTION_PREFIX + row(1)) // Second element of the triple
      out.append(ACTION_PREFIX + row(2)) // Third element of the triple
    }

    //## println(out.mkString(", "))

    // Convert to a set to remove duplicates
    return out.toSet.toArray.sorted
  }

  // Precache all queries
  private def mkPrecachedQueries(): Map[String, String] = {
    val out = mutable.Map[String, String]()

    for (queryStr <- this.mkValidActions()) {
      val queryResults = this.runQuery(queryStr)

      val os = new StringBuilder()
      // Step 3: Check that we have a non-zero number of results
      if (queryResults.length == 0) {
        os.append("No results found.")
      } else {
        // Step 4: Return the results
        os.append("The results are:\n ")
        for (result <- queryResults) {
          os.append(result.mkString(" ") + ".\n ")
        }
      }

      // Store query in cache
      out(queryStr) = os.toString()
    }

    // Return
    out.toMap
  }

  // Run queries
  def runQuery(queryStr:String): Array[Array[String]] = {
    val out = new ArrayBuffer[Array[String]]
    val queryStrSanitized = queryStr.trim().toLowerCase

    // For each row in the knowledge base
    for (row <- ModuleKnowledgeBaseTWC.knowledgeBaseRows) {
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