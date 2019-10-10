object PartitionProblem extends App {
  /*
   * The partition functions returns whether it is possible
   * to divide the n first numbers in the set such that both partitions have the same sum
   * with the added constraint that the first sum is offset by x and the second by y.
   */

  /*
  Q2
   */
  var cache = collection.mutable.Map.empty[(Int, Int, Int), Boolean]
  var i = Seq(5, 12, -2, -7, 2, -10)
  var n = i.length // Take into account indexing starts at 0.
  println("Solution for Q2: " + Partition(n, 0, 0))
  /*
  Q3
   */
  cache = collection.mutable.Map.empty[(Int, Int, Int), Boolean]
  i = Seq(15, 6, -2, -8, 16, 56, -25, 22, 16, -10, -2)
  n = i.length
  println("Solution for Q3: " + Partition(n, 0, 0))
  def Partition(n: Int, x: Int, y: Int): Boolean = {
    if (n == 0) {
      x==y
    } else {
      Partition_(n-1, x + i(n-1), y) || Partition_(n-1, x, y + i(n-1)) // We have to adapt indices because they start at 0 in Scala.
    }
  }
  def Partition_(n: Int, x: Int, y: Int): Boolean = cache.getOrElseUpdate((n, x, y), Partition(n, x, y))
}
