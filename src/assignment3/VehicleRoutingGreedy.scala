package assignment3

import scala.io.Source
import scala.util.control.Breaks._

import scala.math
import java.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object VehicleRoutingGreedy extends App {
  // val src = Source.fromFile("src/assignment3/data/instanceA.txt")
  val src = Source.fromFile(args(0))
  val line = src.getLines().take(1).reduceLeft(_ + " " + _ )

  var array = line.split("[ ,\t]").toList
  val n = array(0).toInt // n customers
  val m = array(1).toInt // m vehicles
  val W = array(2).toInt // W is the maximum capacity
  var items = Array.ofDim[Double](n,5) // n items [weight, x, y]
  var routeList = new Array[List[Int]](m) // list of the customers for each car
  var distances = Array.ofDim[Int](n,n) // matrix of the differences in the distances
  var totalDistance = 0 // minimal distance find so far
  var assigned = new Array[Int](n) // assigned customers
  var routeListWeight = new Array[Int](m) // weight of each car
  var routeListDistance = new Array[Int](m) // weight of each car

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
    items(p)(3) = p
    items(p)(4) = Math.atan2(items(p)(2), items(p)(1))
    p = p + 1
  }

  src.close()

  def compute(){
    for (i <- 0 until n) {
      for (j <- 0 until i) {
        val dx = items(i)(1) - items(j)(1)
        val dy = items(i)(2) - items(j)(2)
        val dist = Math.sqrt(dx * dx + dy * dy)
        val normDist = Math.round(dist).toInt
        distances(i)(j) = normDist
        distances(j)(i) = normDist
      }
    }
  }

  compute()

  items = items.sortBy(_(4))(Ordering[Double])

  for (elem <- 0 until n) {
    val it = items(elem)(3).toInt
    if (it != 0) {
      breakable {
        for (car <- 0 until m; if routeListWeight(car) + items(elem)(0) <= W) {
            routeList(car) = it +: routeList(car)
            routeListWeight(car) += items(elem)(0).toInt
            assigned(it) = 1
            break
        }
      }
    }
  }

  def addZero(): Unit = {
    for (i <- routeList.indices) routeList(i) = 0 +: routeList(i) :+ 0
  }

  addZero()

  for (car <- 0 until m) {
    var prev = 0
    if (routeList(car) != Nil) {
      for (elem <- routeList(car)) {
        totalDistance += distances(prev)(elem)
        prev = elem
      }
    }
  }

  def str(): Unit = {
    for (elem <- 0 until m) {
      println()
      for (car <- routeList(elem)) print(car + " ")
    }
  }

  print(totalDistance)
  str()
}
