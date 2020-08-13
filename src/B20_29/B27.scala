import com.sun.corba.se.impl.orbutil.graph.Graph

object B27 {
  object P1 {
    def canBeEqual(target: Array[Int], arr: Array[Int]): Boolean = {
      target.toList.sorted == arr.toList.sorted
    }
  }
  object P2 {
    def hasAllCodes(s: String, k: Int): Boolean = {
      (0 to s.length - k).toList.map(i =>s.slice(i,i+k)).toSet.size == (1 << k)
    }
  }
  object P3 {
    def checkIfPrerequisite(n: Int, prerequisites: Array[Array[Int]], queries: Array[Array[Int]]): Array[Boolean] = {
      val g = new Graph[Int]()
      prerequisites foreach {case Array(i,j) => g.add(j, i)}
      def find(src:List[Int], dst:Int, vis:Set[Int]):Boolean = src match {
        case Nil => vis.contains(dst)
        case h::t => if(vis.contains(h)) find(t, dst, vis) else if(h == dst) true else
          find(t ++ (g.get(h) filterNot vis.contains), dst, vis ++ g.get(h))
      }
      queries map {case Array(dst, src)=> find(List(src), dst, Set(src))}
    }
  }
  object P4 {
    val inf = 1e9.toInt
    def cherryPickup(grid: Array[Array[Int]]): Int = {
      val n = grid(0).length
      val dp = Array.fill(grid.length, n, n)(-inf)
      dp(0)(0)(n-1) = grid(0)(0) + grid(0).last
      for{
        row <- 0 until grid.length -1
        c1 <- 0 until n
        c2 <- 0 until n
        d1 <- -1 + c1 to 1 + c1
        d2 <- -1 + c2 to 1 + c2
        if d1 >= 0 && d1 < n
        if d2 >= 0 && d2 < n
      } if(d1 == d2) dp(row+1)(d1)(d2) = dp(row+1)(d1)(d2) max (dp(row)(c1)(c2) + grid(row+1)(d1)) else
        dp(row+1)(d1)(d2) = dp(row+1)(d1)(d2) max (dp(row)(c1)(c2) + grid(row+1)(d1)+ grid(row+1)(d2))
      dp.last.flatten.max
    }
  }
}
