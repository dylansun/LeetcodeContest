object C173 {
  object P1 {
    def removePalindromeSub(s: String): Int = s match {
      case "" => 0
      case _ => 2 - (if(s == s.reverse) 1 else 0)
    }
  }

  object P2 {
    def filterRestaurants(restaurants: Array[Array[Int]], veganFriendly: Int, maxPrice: Int, maxDistance: Int): List[Int] = {
      restaurants
        .filter {case Array(id, _, flag, p, d) =>
          flag >= veganFriendly &&
            p <= maxPrice &&
            d <= maxDistance
        }.sortBy {case Array(id, rate,_,_,_) => (-rate, -id)}.map{ A => A.head}.toList
    }
  }


  object P3 {
    // distance matrix
    // dist(i,j)
    def findTheCity(n: Int, edges: Array[Array[Int]], M: Int): Int = {
      val dist = Array.fill(n,n)(10000000)
      (0 until n) foreach {i => dist(i)(i) = 0}
      edges foreach {case Array(i,j,d) => dist(i)(j) = d; dist(j)(i) = d}
      for{
        k <- 0 until n
        i <- 0 until n
        j <- 0 until n
      }  dist(i)(j) =dist(i)(j) min (dist(i)(k) + dist(k)(j))
      dist.map{x => x.count(_<=M)}.zipWithIndex.sortBy{case (x, id) => (x, -id)}.head._2
    }
  }

  object P4 {
    def minDifficulty(A: Array[Int], d: Int): Int = {
      val n = A.length
      val P = Array.fill(n, n)(0)
      for{i <- 0 until n; j <- i until n} P(i)(j) = A.slice(i,j+1).max
      val dp = Array.fill(n+1,d+1)(-1)
      for{i <- 1 to n} dp(i)(1) = P(0)(i-1)
      for{k <- 2 to d; i <- 1 to n if i >= k}
        dp(i)(k) = (for{ii <- k-1 until i } yield dp(ii)(k-1) + P(ii)(i-1)).min
      dp(n)(d)
    }
  }
}
