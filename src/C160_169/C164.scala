package C160_169

/**
  * Created by lilisun on 11/24/19.
  */
object C164 {
  object P1 {
    case class P(x:Int, y:Int)
    def abs(x:Int):Int = Math.abs(x)
    // (a, b) -> (c ,d)
    // (0,0) -> (c-a, d-b)
    // (dx + dy - (dx min dy)) = dx max dy
    def dist(p1:P, p2:P):Int = abs(p1.x - p2.x) max abs(p1.y - p2.y)
    def minTimeToVisitAllPoints(points: Array[Array[Int]]): Int = {
      def f(l:List[P], acc:Int = 0):Int = l match {
        case p1::p2::t => f(p2::t, acc + dist(p1,p2))
        case _ => acc
      }
      f(points.toList.map{case Array(x,y) => P(x,y)})
    }
  }

  object P2 {
    def countServers(grid: Array[Array[Int]]): Int = {
      val m = grid.length
      val n = grid(0).length
      val row = Array.fill(m)(0)
      val col = Array.fill(n)(0)
      for{
        i <- 0 until m
        j <- 0 until n
        if grid(i)(j) == 1
      } {
        row(i) += 1
        col(j) += 1
      }
      var cnt = 0
      for{
        i <- 0 until m
        j <- 0 until n
        if grid(i)(j) == 1
        if row(i) >=2 || col(j) >= 2
      } cnt += 1
      cnt
    }
  }

  object P3 {
    type LS = List[String]
    def suggestedProducts(A: Array[String], w: String): List[List[String]] = {
      val n = w.length
      def f(l:LS, i:Int, acc:List[LS]):List[LS] = if(i >= n) acc.reverse else {
        val nl = l filter {str => str.startsWith(w.slice(0,i+1))}
        if(nl.length > 3) f(nl, i+1, nl.slice(0,3)::acc)
        else f(nl, i+1, nl::acc)
      }
      f(A.toList.sorted, 0, Nil)
    }
  }

  object P4 {
    val mod = (1e9).toInt + 7
    def numWays(steps: Int, n: Int): Int = {
      val bound = 501 min n
      val dp = Array.fill(501, bound)(BigInt(0))
      val zero = BigInt(0)
      dp(0)(0) = 1
      for{i <- 1 to steps}{
        for{j <- 0 until bound}{
          dp(i)(j) =(if(j-1 < 0) zero else dp(i-1)(j-1))  +
            dp(i-1)(j) +
            (if(j+1 >= bound) zero else dp(i-1)(j+1))
          dp(i)(j) = dp(i)(j) % mod
        }
      }
      dp(steps)(0).toInt
    }
  }
}
