object MatrixChainMultiplication extends App {
  /*
   * Operation(i, j) determines the minimum number of operations needed
   * to compute the product of all the matrices from i to j included.
   */


  /*
  Q2
   */
  var cache = collection.mutable.Map.empty[(Int,Int),Int]
  var n = 7
  var dim = Seq(1, 2, 100, 42, 37, 21, 62, 1)
  println("Solution for Q2: " + Operation(0, n-1))
  /*
  Q3
   */
  cache = collection.mutable.Map.empty[(Int,Int),Int]
  n = 7
  dim = Seq(1, 6, 25, 1000, 26, 1258, 99, 1)
  println("Solution for Q3: " + Operation(0, n-1))
  def Operation(i: Int, j: Int): Int = {
    if (i==j) {
      0
    } else {
      var m = Int.MaxValue
      var s = 0
      for (s <- i until j){
        m = m.min(Operation_(i,s) + Operation_(s+1,j) + dim(i) * dim(s+1) * dim(j+1))
      }
      m
    }
  }
  def Operation_(i: Int, j: Int): Int = cache.getOrElseUpdate((i, j), Operation(i, j))
}
