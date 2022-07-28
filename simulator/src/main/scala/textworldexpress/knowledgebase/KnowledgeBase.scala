package textworldexpress.knowledgebase

/*
 * Prototype for any knowledge graph
 */
abstract class KnowledgeBase {
  // The row data for the table
  val rows:Array[Array[String]]

  // A method of querying the graph
  def queryKB(queryStr:String):KBResult

  def queryKBColumn(queryStr:String, colIdx:Int):KBResult

  def getMaxColumns():Int = {
    return rows.map(_.length).max
  }

}



/*
 * Generator
 */
object KnowledgeBase {
  val KB_TWC_GOLD = "kg-twc-gold"

  def mkKnowledgeBase(kbName:String):KnowledgeBase = {

    if (kbName == KB_TWC_GOLD) return new TWCKnowledgeBaseGold()

    throw new RuntimeException("ERROR: KnowledgeBase.mkKnowledgeBase(): Unknown knowledge base requested (" + kbName + ")")
  }


}