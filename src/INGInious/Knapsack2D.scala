object Knapsack2D extends App {
  /*
   * items contains elements of the form (value, weight, volume).
   * Cost(n, W, V) returns the maximum value of a set of items chosen from 1 to n
   * such that the weight does not exceed W and the volume does not exceed V.
   */

  /*
  Q2
   */
  var items = Array((1, 1, 2), (2, 3, 4), (5, 5, 3), (3, 2, 5), (2, 4, 7), (4, 3, 2), (6, 5, 6))
  var W = 10
  var V = 15
  var cache = collection.mutable.Map.empty[(Int, Int, Int), Int]
  println("Solution for Q2: " + Cost(items.length-1, W, V))
  /*
  Q3
  */
  items = Array((10, 45, 29), (42, 10, 40), (23, 26, 26), (25, 42, 21), (22, 58, 12), (1, 20, 14), (2, 30, 30), (35, 15, 10), (30, 10, 13))
  W = 100
  V = 75
  cache = collection.mutable.Map.empty[(Int, Int, Int), Int]
  println("Solution for Q3: " + Cost(items.length-1, W, V))

  def Cost(n: Int, W: Int, V: Int): Int = {
    if (n==0) {
      0
    } else {
      val (un, wn, vn) = items(n)
      if (wn > W || vn > V)
        Cost_(n-1, W, V)
      else
        (un + Cost_(n-1, W-wn, V-vn)).max(Cost_(n-1, W, V))
    }
  }
  def Cost_(n: Int, W: Int, V: Int): Int = cache.getOrElseUpdate((n, W, V), Cost(n, W, V))
}
