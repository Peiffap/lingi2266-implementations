package assignment1

import scala.io.Source

object KnapsackDP extends App {
  // val lines = Source.fromFile("src/assignment1/data/instanceB.txt").getLines.reduceLeft(_ + " " + _)
  val lines = Source.fromFile(args(0)).getLines.reduceLeft(_ + " " + _)
  var vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  val pairs = vals.sliding(2,2).toList
  val n::c::Nil = pairs.head
  var items = pairs.tail.map{case v::w::Nil => (v,w)}.toArray

  val cache = collection.mutable.Map.empty[(Int, Int), Int]
  val tkn = collection.mutable.Map.empty[(Int, Int), Boolean]
  def O(j: Int, k: Int): Int = {
    if (j < 0) 0
    else {
      val (vj, wj) = items(j)
      if (wj > k) {
        tkn.update((j, k), false)
        O_(j-1,k)
      }
      else {
        if (O_(j-1,k) > vj + O_(j-1,k-wj)) {
          tkn.update((j, k), false)
          O_(j-1,k)
        } else {
          tkn.update((j, k), true)
          vj + O_(j-1,k-wj)
        }
      }
    }
  }
  def O_(j: Int, k: Int): Int = cache.getOrElseUpdate((j,k), O(j,k))
  println(println(O(n-1, c)))
  var cleft = c
  var s: String = ""
  if (tkn.getOrElseUpdate((n-1, cleft), false)) { // gOEU is needed with Options, never happens.
    cleft = cleft - items(n - 1)._2
    s = "1"
  } else {
    s = "0"
  }
  for (i <- (0 until n-1).reverse) {
    if (tkn.getOrElseUpdate((i, cleft), false)) { // gOEU is needed with Options, never happens.
      cleft = cleft - items(i)._2
      s = "1 " + s
    } else {
      s = "0 " + s
    }
  }
  println(s)
}
