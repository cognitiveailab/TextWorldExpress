package textworldexpress.preprocessing

import textworldexpress.objects.{BundleOfObjects, FastObject}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

object GenerateSortingProblems {

  def main(args:Array[String]): Unit = {

    val setSize:Int = 25                              // 25 per operator (100 total across the 4 operators)
    val numUniqueProblemsToGenerate:Int = setSize*3   // *3 for train, dev, and test
    val maxInt = 50
    val r = new Random(2051)

    // Train, dev, and test sets
    val train = new ArrayBuffer[SortingProblem]
    val dev = new ArrayBuffer[SortingProblem]
    val test = new ArrayBuffer[SortingProblem]



    // Raw quantities
    val objectNamesTrain  = Array(("apple", "apples"), ("orange", "oranges"), ("grape", "grapes"), ("tangerine", "tangerines"), ("banana", "bananas"), ("pineapple", "pineapples"), ("papaya", "papayas"), ("peach", "peaches"), ("strawberry", "strawberries"), ("grapefruit", "grapefruits") )
    val objectNamesDev    = Array(("broccoli", "brocollis"), ("onion", "onions"), ("cucumber", "cucumbers"), ("potato", "potatoes"), ("cucumber", "cucumbers"), ("coconut", "coconuts"), ("watermelon", "watermelons"), ("mango", "mangos"), ("olive", "olives"), ("lime", "limes"), ("pear", "pears") )
    val objectNamesTest   = Array(("pepper", "peppers"), ("tomato", "tomatoes"), ("eggplant", "eggplants"), ("squash", "squashes"), ("pumpkin", "pumpkins"), ("pea", "peas"), ("avocado", "avocados"), ("cabbage", "cabbages"), ("prune", "prunes"), ("blueberry", "blueberries") )

    for (fold <- Array("train", "dev", "test")) {
      val objectNames = if (fold == "train") { objectNamesTrain } else if (fold == "dev") { objectNamesDev } else if (fold == "test") { objectNamesTest } else { throw new RuntimeException("ERROR: Unknown fold (" + fold + ")") }

      val problems = new ArrayBuffer[SortingProblem]()
      for (i <- 0 until setSize) {
        val problemSize = r.nextInt(3) + 3

        val items = new ArrayBuffer[ItemQuantity]
        val existingQuantities = mutable.Set[Int]()
        for (j <- 0 until problemSize) {
          val objectNamesShuffled = r.shuffle(objectNames.toList).toArray

          // Make a unique quantity, that hasn't been used in this problem before
          var quantity = r.nextInt(maxInt) + 1
          while (existingQuantities.contains(quantity)) {
            quantity = r.nextInt(maxInt) + 1
          }
          existingQuantities.add(quantity)

          val objName = objectNamesShuffled(0)

          var name = ""
          if (quantity == 1) {
            name = objName._1
          } else {
            name = objName._2
          }

          items.append( new ItemQuantity(name, quantity, SISuffix = "") )
        }

        problems.append( new SortingProblem(items.toArray) )
      }

      if (fold == "train") {
        train.insertAll(train.size, problems)
      } else if (fold == "dev") {
        dev.insertAll(dev.size, problems)
      } else if (fold == "test") {
        test.insertAll(test.size, problems)
      } else {
        throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
      }

    }



    // Material Names
    val materialNamesTrain  = Array("wood", "oak", "cedar", "marble", "brick")
    val materialNamesDev    = Array("acrylic", "nylon", "plastic", "concrete", "steel")
    val materialNamesTest   = Array("brass", "aluminum", "metal", "rubber", "glass")

    for (property <- Array("length", "mass", "volume")) {
      for (fold <- Array("train", "dev", "test")) {
        val materialNames = if (fold == "train") {
          materialNamesTrain
        } else if (fold == "dev") {
          materialNamesDev
        } else if (fold == "test") {
          materialNamesTest
        } else {
          throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
        }

        val problems = new ArrayBuffer[SortingProblem]()
        for (i <- 0 until setSize) {
          val problemSize = r.nextInt(3) + 3

          val items = new ArrayBuffer[ItemQuantity]
          val existingQuantities = mutable.Set[Int]()
          for (j <- 0 until problemSize) {
            val materialNamesShuffled = r.shuffle(materialNames.toList).toArray

            // Make a unique quantity, that hasn't been used in this problem before
            var quantity = r.nextInt(maxInt) + 1
            while (existingQuantities.contains(quantity)) {
              quantity = r.nextInt(maxInt) + 1
            }
            existingQuantities.add(quantity)

            val materialName = materialNamesShuffled(0)

            // Next: Generate random suffix
            var suffixes = Array("")
            if (property == "length") {
              //suffixes = Array("mm", "cm", "m", "km")
              suffixes = Array("mm", "cm", "m")   // Might not be super plausible to pick up kilometers of material
            } else if (property == "mass") {
              suffixes = Array("mg", "g", "kg")
            } else if (property == "volume") {
              suffixes = Array("ml", "l")
            } else {
              throw new RuntimeException("ERROR: Unknown property (" + property + ")")
            }
            val shuffledSuffixes = r.shuffle(suffixes.toList)
            val suffixToUse = shuffledSuffixes(0)

            items.append(new ItemQuantity(materialName, quantity, suffixToUse))
          }

          problems.append(new SortingProblem(items.toArray))
        }

        if (fold == "train") {
          train.insertAll(train.size, problems)
        } else if (fold == "dev") {
          dev.insertAll(dev.size, problems)
        } else if (fold == "test") {
          test.insertAll(test.size, problems)
        } else {
          throw new RuntimeException("ERROR: Unknown fold (" + fold + ")")
        }

      }
    }


    // Show train/dev/test sets

    println ("// Train ")
    val trainShuffled = r.shuffle(train)
    val trainArrayStr = this.mkArray(trainShuffled.toArray)
    println("val trainSet = " + trainArrayStr)
    println ("")

    println ("// Dev ")
    val devShuffled = r.shuffle(dev)
    val devArrayStr = this.mkArray(devShuffled.toArray)
    println("val devSet = " + devArrayStr)
    println ("")

    println ("// Test ")
    val testShuffled = r.shuffle(test)
    val testArrayStr = this.mkArray(testShuffled.toArray)
    println("val testSet = " + testArrayStr)
    println("")

  }

  def mkArray(in:Array[SortingProblem]): String = {

    // Display
    val elements = new ArrayBuffer[String]
    for (i <- 0 until in.length) {
      //println (i + ":\t" + in(i).toString() + "     (result: " + in(i).generateResult().get + ")")
      elements.append( in(i).toCodeString() )
    }

    return "Array(\n\t" + elements.mkString(",\n\t") + "\n\t)"

  }

}


// Storage class for an item of a given quantity
// Name = "string", quantity = "10", SISuffix = "cm" == 10 cm of string
class ItemQuantity(val name:String, val quantity:Long, val SISuffix:String) {

  def getFullQuantity():Long = {

    SISuffix.toLowerCase match {
      // Length (base unit mm)
      case "mm" => return quantity * 1
      case "cm" => return quantity * 10
      case "m" => return quantity * 1000
      case "km" => return quantity * 1000000

      // Mass (base unit milligrams)
      case "mg" => return quantity * 1
      case "g" => return quantity * 1000
      case "kg" => return quantity * 1000000

      // Volume (base unit milliliter)
      case "ml" => return quantity * 1
      case "l" => return quantity * 1000

      case _ => {
        // Unknown or none
        return quantity
      }
    }

  }

  def toCodeString():String = {
    return "new ItemQuantity(\"" + name + "\", " + quantity + ", \"" + SISuffix + "\")"
  }

}

class SortingProblem(inputItems:Array[ItemQuantity]) {
  val orderedItems = this.getOrderedItems()

  private def getOrderedItems():Array[FastObject] = {
    // Step 1: Sort
    val sorted = this.inputItems.sortBy(_.getFullQuantity())

    // Step 2: Convert to FastObjects
    val out = new ArrayBuffer[FastObject]
    for (iq <- sorted) {
      // Make object name
      var name = ""
      if (iq.SISuffix.length > 0) {
        name = iq.quantity + iq.SISuffix + " of " + iq.name
      } else {
        name = iq.quantity + " " + iq.name
      }

      // Make FastObject
      val obj = new BundleOfObjects(name)
      out.append(obj)
    }

    return out.toArray
  }

  def toCodeString():String = {
    return "new SortingProblem(inputItems = Array(" + this.inputItems.map(_.toCodeString()).mkString(", ") + ") )"
  }

}
