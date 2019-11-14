package assignment1

import assignment1.BnB.{Node, PQueue, Stack}

import scala.io.Source

/**
 * @author Pierre Schaus
 *         Gilles Peiffer
 */
object KnapsackBnB extends App {
  // val src = Source.fromFile("src/assignment1/data/instanceB.txt")
  val src = Source.fromFile(args(0))
  val lines = src.getLines.reduceLeft(_ + " " + _)
  src.close()
  var vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  val pairs = vals.sliding(2,2).toList
  val n::c::Nil = pairs.head
  var items = pairs.tail.map{case v::w::Nil => (v,w)}.toArray

  //val items = Array((1,1),(6,2),(18,5),(22,6),(28,7)) // (value,weight)
  //val c = 11

  var ov: Seq[Int] = Seq()
  var ow: Seq[Int] = Seq()

  for (i <- items.indices.reverse) {
    ov = items(i)._1 +: ov
    ow = items(i)._2 +: ow
  }

  // sort items by increasing value/weight ratio
  scala.util.Sorting.quickSort(items)(Ordering.by { case (v, w) => -v}) // -v.toDouble / w for other instances

  val root = new KnapsackNode(
    items = items,
    obj = 0,
    selected = Nil,
    capa = c,
    selectable = items.indices.toList,
    ws = ow,
    vs = ov)

  val bestSol = BnB.solve(new Stack(root))
  println(bestSol.get)
}

class KnapsackNode(val items: Array[(Int, Int)], // (value,weight)
                   val obj: Int,
                   val selected: List[Int],
                   val capa: Int,
                   val selectable: List[Int],
                   val ws: Seq[Int],
                   val vs: Seq[Int]) extends Node {

  override def feasible: Boolean = true

  override def objective: Double = obj

  def weight(i: Int): Int = items(i)._2
  def value(i: Int): Int = items(i)._1

  override val upperBound: Double = {
    // Linear relaxation.
    var i: Int = 0
    var ub: Double = obj
    var c: Int = capa
    if (c > 0) {
      while (i < selectable.length
          && c >= weight(selectable(i))) {
        c = c - weight(selectable(i))
        ub = ub + value(selectable(i))
        i += 1
      }
      if (i < selectable.length) {
        val d: Double = c.toDouble / weight(selectable(i))
        ub = ub + value(selectable(i)) * d
      }
    }
    ub
  }

  override def branch(): Seq[Node] = {
    if (selectable.isEmpty) Seq.empty
    else {
      val i = selectable.head
      val (v,w) = items(i)
      val l = new KnapsackNode(items, obj + v, i :: selected, capa - w, selectable.tail, ws, vs)
      val r = new KnapsackNode(items, obj, selected, capa, selectable.tail, ws, vs)
      if (w <= capa) Seq(l,r)
      else Seq(r)
    }
  }

  // findIndex helps to recover the new (post-sort) index of the item with weight w and value v.
  def findIndex(w: Int, v: Int): Int = {
    for (i <- items.indices; if weight(i) == w && value(i) == v) return i
    -1 // This never happens, but otherwise the code won't run.
  }

  // toString tells the program how to follow the output format for the assignment.
  override def toString: String = {
    var s: String = obj + "\n"
    for (i <- 0 until ws.length - 1) {
      if (selected.contains(findIndex(ws(i), vs(i)))) {
        s = s + "1 "
      } else {
        s = s + "0 "
      }
    }
    if (selected.contains(findIndex(ws(ws.length-1), vs(vs.length-1)))) {
      s = s + "1"
    } else {
      s = s + "0"
    }
    s
  }
}

