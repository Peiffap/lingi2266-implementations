package assignment1

import scala.collection.mutable


/**
 * @author Pierre Schaus
 *         Gilles Peiffer
 */
object BnB {

  trait Node {
    def feasible: Boolean
    def objective: Double
    def branch(): Seq[Node]
    def upperBound: Double
  }

  trait OpenNodes {
    def add(n: Node)
    def remove(): Node
    def isEmpty: Boolean
  }

  class PQueue(root: Node) extends OpenNodes {
    val pq: mutable.PriorityQueue[Node] = mutable.PriorityQueue[Node](root)(Ordering.by(n => -n.upperBound))
    override def add(n: Node): Unit = pq.enqueue(n)
    override def remove(): Node = pq.dequeue()
    override def isEmpty: Boolean = pq.isEmpty
  }

  class Stack(root: Node) extends OpenNodes {
    var stack: List[Node] = List[Node](root)
    override def add(n: Node): Unit = {
      stack = n :: stack
    }
    override def remove(): Node = {
      val h = stack.head
      stack = stack.tail
      h
    }
    override def isEmpty: Boolean = stack.isEmpty
  }


  def solve(open: OpenNodes): Option[Node] = {
    var bestNode: Option[Node] = None
    var bestObj = Double.MinValue

    while (!open.isEmpty) {
      val n: Node = open.remove()
      if (n.feasible && n.objective > bestObj) {
        bestObj = n.objective
        bestNode = Some(n)
      }
      for (c <- n.branch().reverse; if c.upperBound > bestObj) {
        open.add(c)
      }
    }
    bestNode
  }
}
