import scala.collection.mutable.{ArrayBuffer => AB}
object C195 {
  object P1 {
    val M = Map('N'->Point(0,-1),'S'->Point(0,1), 'E'->Point(1, 0), 'W'-> Point(-1,0))
    def isPathCrossing(path: String): Boolean = {
      def f(l:List[Char], cur:Point, vis:Set[Point]):Boolean = l match {
        case Nil => false
        case h::t => if(vis.contains(cur+M(h)))true else f(t, cur+M(h), vis + (cur + M(h)))
      }
      f(path.toList, Point(0,0), Set(Point(0,0)))
    }
  }
  object P2 {
    def canArrange(arr: Array[Int], k: Int): Boolean = {
      val A = Array.fill(k)(0)
      for{x <- arr} A(x % k) += 1
      for{i <- 1 until k if A(i) != A(k-i)} return false
      (k % 2 == 0 && A(k/2) % 2==0) || (k % 2 != 0)
    }
  }
  object P3 {
    def numSubseq(nums: Array[Int], target: Int): Int = {
      val M = nums.max
      val A = Array.fill(M+1)(0)
      val B = Array.fill(M+1)(0)
      nums foreach {x => A(x) += 1}
      for{i <- 1 to M} B(i) = B(i-1) + A(i)
      var ret = 0
      for{
        x <- 1 to M
        if A(x) > 0
        if target - x >= x
        tmp = M min (target - x)
        f = (1 << A(x)) -1
      } ret = SafeCal.add(ret, SafeCal.mul(f, 1 << (B(tmp) - B(x))))
      ret
    }
  }
  object P4 {
    def findMaxValueOfEquation(points: Array[Array[Int]], k: Int): Int = {
      val ab = AB[Int]()
      var pos = 0
      var ret = 0
      for{i <- points.indices} points(i) match{case Array(xi,yi) =>
        while(pos < i && xi-points(pos)(0) > k){
          Bisect.remove(ab, points(pos)(1) - points(pos)(0))
          pos += 1
        }
        if(ab.nonEmpty){ ret = ret max (xi+yi + ab.last)}
        Bisect.insort(ab, -xi+yi)
      }
      ret
    }
  }
}
