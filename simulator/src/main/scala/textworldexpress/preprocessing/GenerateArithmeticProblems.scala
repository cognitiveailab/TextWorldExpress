package textworldexpress.preprocessing

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

object GenerateArithmeticProblems {

  def main(args:Array[String]): Unit = {

    val numUniqueProblemsToGenerate:Int = 60
    val maxInt = 50
    val r = new Random(2501)


    val operators = Array("+", "-", "*", "/")

    for (operator <- operators) {
      val existingProblems = new ArrayBuffer[ArithmeticProblem]

      while (existingProblems.length < numUniqueProblemsToGenerate) {
        // Randomly generate a problem
        var num1 = r.nextInt(maxInt-1)+1
        val num2 = r.nextInt(maxInt-1)+1
        if (operator == "/") num1 += 1    // Prevent having divisors of 1 for division
        val problem = new ArithmeticProblem(num1, num2, operator)
        // Check that the problem is valid (i.e. an integer result)
        val isValid = problem.generateResult().isDefined


        if (isValid) {
          // Check for duplicates
          breakable {
            for (existing <- existingProblems) {
              if (existing == problem) break()
            }
            // If we reach here, the problem is not a duplicate
            existingProblems.append(problem)
          }

        }

      }

      // Display
      for (i <- 0 until existingProblems.length) {
        println (i + ":\t" + existingProblems(i).toString() + "     (result: " + existingProblems(i).generateResult().get + ")")
      }
      println ("")
    }



  }


}


class ArithmeticProblem(val num1:Int, val num2:Int, val operation:String) {

  def generateText():String = {
    if (operation == "+") return "add " + num1 + " and " + num2
    if (operation == "-") return "subtract " + num2 + " from " + num1
    if (operation == "*") return "multipy " + num1 + " and " + num2
    if (operation == "/") return "divide " + num2 + " by " + num1

    // If we reach here, unknown operation
    throw new RuntimeException("ERROR: Unknown operation (" + operation + ")")
  }

  def generateResult():Option[Int] = {
    if (operation == "+") return Some(num1+num2)
    if (operation == "-") {
      // Result must be positive
      val result = num1-num2
      if (result > 0) {
        return Some(num1 - num2)
      } else {
        return None
      }
    }
    if (operation == "*") return Some(num1*num2)
    if (operation == "/") {
      val result = num2.toDouble / num1.toDouble
      if (result % 1 == 0) {
        // Result is an integer -- so it's valid
        return Some(result.toInt)
      } else {
        return None
      }
    }

    // If we reach here, unknown operation
    throw new RuntimeException("ERROR: Unknown operation (" + operation + ")")
  }

  def generateDistractors():Array[Int] = {
    // True is addition
    if (operation == "+") return Array(num1-num2, num2-num1, num1*num2, math.round(num2/num1).toInt, math.round(num1/num2).toInt )    // True: num1+num2
    if (operation == "-") return Array(num1+num2, num2-num1, num1*num2, math.round(num2/num1).toInt, math.round(num1/num2).toInt )    // True: num1-num2
    if (operation == "*") return Array(num1+num2, num1-num2, num2-num1, math.round(num2/num1).toInt, math.round(num1/num2).toInt )    // True: num1*num2
    if (operation == "/") return Array(num1+num2, num1-num2, num2-num1, num1*num2, math.round(num2/num1).toInt )                      // True: num1/num2

    // If we reach here, unknown operation
    throw new RuntimeException("ERROR: Unknown operation (" + operation + ")")
  }

  /*
   * Operators
   */
  // Equality
  override def equals(that:Any):Boolean = {
    that match {
      case x:ArithmeticProblem => {
        if (this.operation != x.operation) return false
        if (this.operation == "+") {
          if ((this.num1 == x.num1) && (this.num2 == x.num2)) return true
          if ((this.num1 == x.num2) && (this.num2 == x.num1)) return true
          return false
        }
        if (this.operation == "-") {
          if ((this.num1 == x.num1) && (this.num2 == x.num2)) return true
          return false
        }
        if (this.operation == "*") {
          if ((this.num1 == x.num1) && (this.num2 == x.num2)) return true
          if ((this.num1 == x.num2) && (this.num2 == x.num1)) return true
          return false
        }
        if (this.operation == "/") {
          if ((this.num1 == x.num1) && (this.num2 == x.num2)) return true
          return false
        }
      }
      case _ => return false
    }

    return false
  }

  /*
   * String methods
   */
  override def toString() = this.generateText()

}

