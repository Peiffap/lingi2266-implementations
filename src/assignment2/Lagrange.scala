package assignment2

import scala.io.Source
import scala.language.postfixOps

object Lagrange extends App {
  // val src = Source.fromFile("src/assignment2/data/instanceA.txt")
  val src = Source.fromFile(args(0))
  val line = src.getLines().take(1).reduceLeft(_ + " " + _)
  var array = line.split("[ ,\t]").toList
  var n = array(0).toInt // Number of elements.
  var m = array(1).toInt // Number of sets.
  var items = Array.ofDim[Int](m, n) // Array containing binary variables to determine whether set i contains item j.
  var numberItems = new Array[Int](n)
  var c = new Array[Int](m) // Cost of every set.
  var elementsIn = new Array[List[Int]](m) // elementsIn is an array containing, for every set, the elements it contains.

  var i = 0
  for (line <- src.getLines) {
    val arr = line.split("[ ,\t]").toList
    c(i) = arr(0).toInt
    elementsIn(i) = List.empty
    for (j <- 2 until arr.length) {
      val elem: Int = arr(j).toInt - 1
      items(i)(elem) = 1
      numberItems(elem) = 1
      elementsIn(i) +:= elem
    }
    i += 1
  }
  src.close() // Close the input.

  // If there are items which are not present in any set, the problem is infeasible.
  if (numberItems.contains(0)) {
    println("infeasible")
    System.exit(0)
  }

  // setsContaining is an array containing, for every element, the sets which contain it.
  var setsContaining: Array[List[Int]] = new Array[List[Int]](n)
  for (j <- setsContaining.indices) {
    setsContaining(j) = List.empty
    for (i <- items.indices if items(i)(j) == 1) setsContaining(j) +:= i // Prepend for efficiency.
  }

  var lambda: Array[Double] = new Array[Double](n)
  val maxiter: Int = 5_000_000 // Threshold for iteration.
  val delta: Double = 1e-4 // Threshold for gap.

  var y: Array[Int] = new Array[Int](m) // Decision variables.
  var lb: Double = 0.0 // Lower bound on the objective.

  var bestFeasibleY: Array[Int] = Array.fill[Int](m)(1)
  var bestCost: Int = c.sum // Best-known lower bound objective; we initialize it to the sum of all elements.

  /**
   * Compute a lower bound for the problem.
   * @return a lower bound.
   */
  def lowerBound(): Double = {
    // Iterate.
    for (k <- 1 to maxiter) {
      // Update the parameters.
      val mu = 1.0 / k // Step size.

      // Update the lambdas using the update rule.
      for (j <- lambda.indices) {
        var mul: Int = 1
        for (i <- setsContaining(j) if y(i) == 1) mul -= 1 // Find the sets in which element j appears.
        lambda(j) = Math.max(0, lambda(j) + mu * mul)
      }

      val coeff: Array[Double] = new Array[Double](m) // Coefficients of the decision variables.
      y = new Array[Int](m)
      for (i <- coeff.indices) {
        coeff(i) = c(i)
        for (j <- elementsIn(i)) coeff(i) -= lambda(j) // Update coefficients.
        if (coeff(i) < 0.0) y(i) = 1 // Decide which sets to take.
      }

      lb = 0.0
      for (i <- y.indices if y(i) == 1) lb += c(i)
      for (j <- lambda.indices) {
        var mul: Int = 1
        for (i <- setsContaining(j) if y(i) == 1) mul -= 1 // Find the sets in which element j appears.
        lb += lambda(j) * mul
      }
      lb = Math.ceil(lb)

      // Update best-known feasible solution if needed.
      if (isFeasible()) {
        val yCost: Int = cost()
        if (yCost < bestCost) {
          bestCost = yCost
          bestFeasibleY = copy(y) // Copy is needed because y can still change.
        }
      }
    }
    lb
  }

  lb = lowerBound()
  // If the best lower bound is the same as the best cost, we have found an optimal solution,
  // otherwise we have a lb and a feasible solution.
  println(if (bestCost - lb < delta) "optimum" else "lower bound with solution")
  println(format(lambda))
  println(format(y))
  println(format(bestFeasibleY))

  /**
   * Wrapper for array copy.
   * @param array
   * @return a copy of array.
   */
  def copy(array:Array[Int]): Array[Int] = {
    val newArray = new Array[Int](array.length)
    Array.copy(array, 0, newArray, 0, array.length)
    newArray
  }

  /**
   * Cost of a given solution.
   * @return the cost of y.
   */
  def cost(): Int = {
    var co: Int = 0
    for (i <- y.indices if y(i) == 1) co += c(i)
    co
  }

  /**
   * Feasibility of a set of decision variables.
   * @return the feasibility.
   */
  def isFeasible(): Boolean = {
    // See which elements are in the solution.
    val elemsTaken: Array[Int] = new Array[Int](n) // Initialize array.
    for (i <- y.indices if y(i) == 1; j <- elementsIn(i)) elemsTaken(j) = 1 // Set values to 1 when element is taken.
    !elemsTaken.contains(0)
  }

  /**
   * Format output.
   * @param arr
   * @return formatted output.
   */
  def format(arr: Array[Int]): String = {
    var s: String = new String
    for (i <- 0 until arr.length-1) s = s + arr(i) + " "
    s = s + arr(arr.length-1)
    s
  }

  def format(arr: Array[Double]): String = {
    var s: String = new String
    for (i <- 0 until arr.length-1) s = s + arr(i) + " "
    s = s + arr(arr.length-1)
    s
  }
}