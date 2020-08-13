object B19 {

  object P1 {
    def f(x:Int, n:Int = 0):Int = {
      if(x == 0) n else x % 2 match {
        case 0 => f(x / 2, n+1)
        case 1 => f(x-1, n+1)
      }
    }
    def numberOfSteps (num: Int): Int = f(num)
  }

  object P2 {
    def numOfSubarrays(A: Array[Int], k: Int, threshold: Int): Int = {
      val p = k * threshold
      def g(x:Int):Int = if(x >= p) 1 else 0
      def f(i:Int, cur:Int, n:Int = 0):Int = {
        if(i >= A.length) n
        else if(i == A.length-1) n+g(cur)
        else f(i+1, cur + A(i+1) - A(i-k+1), n + g(cur))
      }

      f(k-1, A.slice(0,k).sum, 0)
    }
  }

  object P3 {
    def angleClock(hour: Int, minutes: Int): Double = {
      // h => 30
      // m => 6
      val d = math.abs(hour * 30.0 + minutes * 0.5 - minutes * 6.0 )
      (360 - d) min d
    }
  }

  // TODO: TLE
  object P4 {
    def minJumps(A: Array[Int]): Int = {
      def inBound(x:Int):Boolean = A.indices.contains(x)
      val visited = Array.fill(A.length)(false)
      visited(A.length-1) =true
      val table = scala.collection.mutable.HashMap[Int, List[Int]]()
      A.indices foreach {i => table.put(A(i), i::table.getOrElse(A(i), Nil))}

      def nei(x:Int):List[Int] = {
        List(x+1,x-1).filter(inBound) ++ table(A(x)).filter{y => y!=x && !visited(y)}
      }
      def f(cur:Set[Int], step:Int = 0):Int = {
        if(visited(0)) step else {
          val next = cur flatMap nei
          next foreach {i => visited(i) = true}
          f(next -- cur, step + 1)
        }
      }
      f(Set(A.length-1), 0)
    }
  }
}
