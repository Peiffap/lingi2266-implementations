package assignment3

import scala.io.Source
import util.control.Breaks._
import java.util


object VehicleRoutingClarkeWright extends App {

  val src = Source.fromFile("src/assignment3/data/instanceA.txt")
  // val src = Source.fromFile(args(0))
  val line = src.getLines().take(1).reduceLeft(_ + " " + _ )
  var array = line.split("[ ,\t]").toList
  val n = array(0).toInt // n customers
  val m = array(1).toInt // m vehicles
  val W = array(2).toInt // W is the maximum capacity
  var items = Array.ofDim[Double](n,3) // n items [weight, x, y]
  var routeList = new Array[List[Int]](m) // list of the customers for each car
  var distances = Array.ofDim[Int](n,n) // matrix of the differences in the distances
  var totalDistance = 0 // minimal distance found so far
  var orderedList = Array.ofDim[Int](((n-1)*(n-2))/2,3) // ordered list of the differences in the distance
  var assigned = new Array[Int](n) // assigned customers
  var routeListWeight = new Array[Int](m) // weight of each car
  var routeListDistance = new Array[Int](m) // distance of each car
  var newArray = new Array[Array[Int]](m)
  var saveArray = new Array[Array[Int]](m)
  var K = 20

  // initiate the routeList
  for (i <- 0 until m) {
    routeList(i) = List.empty
  }

  // read the file
  var p = 0
  for (line <- src.getLines) {
    val arr = line.split("[ ,\t]").toList
    items(p)(0) = arr(0).toInt
    items(p)(1) = arr(1).toDouble
    items(p)(2) = arr(2).toDouble
    p = p + 1
  }

  src.close()
  // finish reading the file

  // compute the pairwise distance matrix between the clients
  def compute(){
    for (i <- 0 until n; j <- 0 until i) {
      val dx = items(i)(1) - items(j)(1)
      val dy = items(i)(2) - items(j)(2)
      val dist = Math.sqrt(dx * dx + dy * dy)
      val normDist = Math.round(dist).toInt
      distances(i)(j) = normDist
      distances(j)(i) = normDist
    }
  }

  def changeAssigned(oldSet: Int, newSet: Int): Unit ={
    for (elem <- 0 until n; if assigned(elem) == oldSet) {
      assigned(elem) = newSet
    }
  }

  compute()

  // make the ordered list (in decreasing order)
  p = 0
  for (i <- 1 until n; j <- i + 1 until n) {
    orderedList(p)(0) = i
    orderedList(p)(1) = j
    orderedList(p)(2) = distances(0)(i) + distances(0)(j) - distances(i)(j)
    p += 1
  }
  orderedList = orderedList.sortBy(_(2))(Ordering[Int].reverse)

  // transform the list in routes
  for (l <- orderedList.indices) {
    val i = orderedList(l)(0)
    val j = orderedList(l)(1)
    val iw = items(i)(0)
    val jw = items(j)(0)
    val ijdist = distances(i)(j)
    if (assigned(i) == 0 && assigned(j) == 0 && iw + jw <= W) { // neither i or j have already been assigned to a route -> create a new route including both i and j
      breakable {
        for (elem <- 0 until m; if routeList(elem) == Nil) {
          routeList(elem) = j +: routeList(elem)
          routeList(elem) = i +: routeList(elem)
          routeListWeight(elem) = iw.toInt + jw.toInt
          routeListDistance(elem) = ijdist
          assigned(i) = elem + 1
          assigned(j) = elem + 1
          break
        }
      }
    } else if (assigned(i) == 0) { // j has already been included in an existing route and that point is not interior to that route
      val elem = assigned(j)-1
      if (routeListWeight(elem) + iw <= W && (routeList(elem).head == j || routeList(elem).last == j)) {
        if (routeList(elem).head == j) routeList(elem) = i +: routeList(elem)
        else routeList(elem) = routeList(elem) :+ i
        routeListWeight(elem) += iw.toInt
        routeListDistance(elem) += ijdist
        assigned(i) = elem + 1
      }
    } else if (assigned(j) == 0) { // i has already been included in an existing route and that point in not interior to that route
      val elem = assigned(i)-1
      if (routeListWeight(elem) + jw <= W && (routeList(elem).head == i || routeList(elem).last == i)) {
        if (routeList(elem).head == i) routeList(elem) = j +: routeList(elem)
        else routeList(elem) = routeList(elem) :+ j
        routeListWeight(elem) += jw.toInt
        routeListDistance(elem) += ijdist
        assigned(j) = elem + 1
      }
    } else { // both i and j are included in two different existing routes and neither point is interior to its route -> two routes are merged
      val elemi = assigned(i) - 1
      val elemj = assigned(j) - 1
      if (assigned(i) != assigned(j) && routeListWeight(elemi) + routeListWeight(elemj) <= W && (routeList(elemi).head == i && routeList(elemj).last == j || routeList(elemi).last == i && routeList(elemj).head == j || routeList(elemi).head == i && routeList(elemj).head == j || routeList(elemi).last == i && routeList(elemj).last == j)) {
        if (routeList(elemi).head == i && routeList(elemj).last == j) routeList(elemi) = routeList(elemj) ++ routeList(elemi)
        else if (routeList(elemi).last == i && routeList(elemj).head == j) routeList(elemi) = routeList(elemi) ++ routeList(elemj)
        else if (routeList(elemi).head == i && routeList(elemj).head == j) routeList(elemi) = routeList(elemj).reverse ++ routeList(elemi)
        else routeList(elemi) = routeList(elemj) ++ routeList(elemi).reverse
        routeList(elemj) = List.empty
        routeListDistance(elemi) += ijdist + routeListDistance(elemj)
        routeListDistance(elemj) = 0
        routeListWeight(elemi) += routeListWeight(elemj)
        routeListWeight(elemj) = 0
        changeAssigned(elemj+1, elemi+1)
      }
    }
  }

  // assign points that were not assigned to individual cars
  for (elem <- 1 until n; if assigned(elem) == 0) {
    val elemw = items(elem)(0)
    breakable {
      for (el <- 0 until m; if routeList(el) == Nil) {
        routeList(el) = elem +: routeList(el)
        routeListWeight(el) = elemw.toInt
        assigned(elem) = el+1
        break
      }
    }
  }

  def str(): Unit ={
    for (elem <- 0 until m) {
      println()
      for (car <- newArray(elem)) {
        print(car + " ")
      }
    }
  }

  def totDist(): Unit = {
    totalDistance = 0
    for (elem <- 0 until m) {
      var prev = 0
      if (newArray(elem) != Nil) {
        for (car <- newArray(elem)) {
          totalDistance += distances(prev)(car)
          prev = car
        }
      }
    }
  }

  def addZero(): Array[Array[Int]] = {
    val newArray = new Array[Array[Int]](m)
    for (i <- routeList.indices){
      routeList(i) = 0 +: routeList(i) :+ 0
      newArray(i) = routeList(i).toArray
    }
    newArray
  }

  def twoOpt(elem : Int, i: Int, j: Int): Unit = {
    val (left, right) = (i min j, i max j)
    assert((i min j) >= 0 && (i max j) < newArray(elem).length)
    var k = 0
    val n = (right - left + 1) / 2
    while (k < n) {
      val tmp = newArray(elem)(left + 1 + k)
      newArray(elem)(left + k + 1) = newArray(elem)(right - k)
      newArray(elem)(right - k) = tmp
      k += 1
    }
  }

  /**
   * @param 0 < i != j < nNodes-1
   * @return cost change of the tour if we were to execute twoOpt(left,right)
   */
  def deltaTwoOpt(elem : Int, i: Int, j: Int): Int = {
    val (left, right) = (i min j, i max j)
    assert((i min j) >= 0 && (i max j) < newArray(elem).length)
    val distLeft = distances(newArray(elem)(left))(newArray(elem)(left+1))
    val distRight = distances(newArray(elem)(right))(newArray(elem)(right+1))
    val newDistLeft = distances(newArray(elem)(left))(newArray(elem)(right))
    val newDistRight = distances(newArray(elem)(left+1))(newArray(elem)(right+1))
    newDistLeft + newDistRight - distLeft - distRight
  }

  def iteration(elem : Int) : Boolean = {
      var bestLeft, bestRight, bestDelta = 0
      for (left <- 0 until newArray(elem).length - 1; right <- left+1 until newArray(elem).length - 1) {
        val delta = deltaTwoOpt(elem, left, right)
        if (delta < bestDelta) {
          bestDelta = delta
          bestLeft = left
          bestRight = right
        }
      }
      twoOpt(elem, bestLeft, bestRight)
      bestDelta < 0
  }

  def optimize(): Unit = {
   for (elem <- 0 until m) {
     var improved = false
     do {
       improved = iteration(elem)
       totDist()
       println(totalDistance)
     } while (improved)
   }
  }

  newArray = addZero()

  saveArray = newArray.clone()

  optimize()

  totDist()

  print(totalDistance)
  var k = 0
  str()
}

