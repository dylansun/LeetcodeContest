import scala.collection.mutable
object B28{
  object P1 {
    def finalPrices(prices: Array[Int]): Array[Int] = {
      def solve(l:List[Int], min_prices:List[Int] = Nil, acc:List[Int] = Nil):List[Int] = l match {
        case Nil => acc
        case h::t =>
          min_prices dropWhile {x => x > h} match {
            case Nil => solve(t, h::Nil, h::acc)
            case h0::t0 => solve(t, h::h0::t0, (h-h0)::acc)
          }
      }
      solve(prices.toList.reverse).toArray
    }
  }
  object P2 {

    class SubrectangleQueries(_rectangle: Array[Array[Int]]) {
      val rec = _rectangle

      def updateSubrectangle(row1: Int, col1: Int, row2: Int, col2: Int, newValue: Int) {
        for {r <- row1 to row2; c <- col1 to col2} rec(r)(c) = newValue
      }

      def getValue(row: Int, col: Int): Int = {
        rec(row)(col)
      }
    }

  }
  object P3 {
    def minSumOfLengths(arr: Array[Int], target: Int): Int = {
      val dp = Array.fill(arr.length)(1e9.toInt)
      val last = mutable.HashMap[Int, Int]()
      last.put(0, -1)
      var cur = 0; var ret = 1e9.toInt
      for{i <- arr.indices}{
        cur += arr(i)
        if(i>0) dp(i) = dp(i-1) min dp(i)
        if(last.contains(cur-target)){
          dp(i) = dp(i) min (i - last(cur-target))
          if(last(cur-target) != -1) ret = ret min (i-last(cur-target) + dp(last(cur-target)))
        }
        last.put(cur, i)
      }

      if(ret >= 1e9.toInt) -1 else ret
    }
  }
  object P4 {
    def minDistance(houses: Array[Int], K: Int): Int = {
      val inf = 1e9.toInt
      val dp = Array.fill(K+1, houses.length)(inf)
      val cache = new Cache[(Int, Int), Int]()
      // k = 1, put it to median
      val A = houses.sorted
      def solve(tup:(Int, Int)):Int = tup match {
        case (i,j) =>
          val median = (i + j) / 2
          (for{ii <- i to j} yield math.abs(A(median) - A(ii))).sum
        case _ => -1
      }
      for {i <- dp(1).indices}
        dp(1)(i) = dp(1)(i) min (for{j <- 0 to i} yield math.abs(A(j) - A(i / 2))).sum

      // k = 2 to K
      for {
        k <- 2 to K
        i <- dp(k).indices
        j <- 0 until i
      } dp(k)(i) = dp(k)(i) min (dp(k-1)(j) + cache.cache(solve, (j+1, i)))
      dp.last.last
    }
  }
}
