object C167 {
  object p1 {
    def f(head:ListNode, acc:Int = 0):Int =
      if(head == null) acc else f(head.next, acc*2 + head.x)
    def getDecimalValue(head: ListNode): Int = f(head)
  }

  object p2 {
    // cur: cur value
    // digit: next digit
    def f(low:Int, high:Int)(cur:Int, digit:Int, acc:List[Int]):List[Int] =
    if(cur > high || digit >= 10) acc else {
      val next = cur * 10 + digit
      if(next >= low && next <= high) f(low,high)(next, digit+1, next::acc)
      else if(next < low) f(low, high)(next, digit+1, acc)
      else acc
    }
    def sequentialDigits(low: Int, high: Int): List[Int] = {
      (1 to 9).toList.flatMap{x => f(low, high)(0, x, Nil)}.sorted
    }
  }
  //
  //
  // a b
  // c d
  // d = (a b c d ) - ( a b) - ( a c ) + a
  // d <- ( d_topleft, d_downright), 可以表示矩形
  // d <- ( d_topleft, length), 正方形
  // area (i,j), matrix (0,0, i,j) area
  // k <- 1 to n, square (i,j, k)
  object p3 {
    // dp(i)(j)(k): top-left (i,j), side-length:K
    // dp(i)(j)(k+1): dp(i)(j)(k) +
    // mat(i+k)(j+k) + row(i,j -> i , j+k-1) + col(i,j -> j, i+k-1)
    // (i,j) foreach dp(i)(j)(k)
    def maxSideLength(mat: Array[Array[Int]], threshold: Int): Int = {
      val m = mat.length
      val n = mat(0).length
      val area = Array.fill(m,n)(0)
      for{
        i <- 0 until m
        j <- 0 until n
      } (i,j) match {
        case (0,0) => area(i)(j) = mat(0)(0)
        case (0,_) => area(i)(j) = area(0)(j-1) + mat(0)(j)
        case (_,0) => area(i)(0) = area(i-1)(0) + mat(i)(0)
        case (_,_) => area(i)(j) = area(i-1)(j) + area(i)(j-1) + mat(i)(j) - area(i-1)(j-1)
      }
      var ans = 0
      for{
        k <- 1 to (m min n)
        i <- 0 until m
        j <- 0 until n
        if i +k -1 < m
        if j +k -1 < n
      } {
        val tmp = (i,j) match {
          case (0,0) => area(k-1)(k-1)
          case (0,_) => area(k-1)(j+k-1) - area(k-1)(j-1)
          case (_,0) => area(i+k-1)(k-1) - area(i-1)(k-1)
          case (_,_) => area(i+k-1)(j+k-1) - area(i+k-1)(j-1) - area(i-1)(j+k-1) + area(i-1)(j-1)
        }
        if(tmp <= threshold) ans = ans max k
      }
      ans
    }
  }

  object p4 {
    case class Pos(x:Int, y:Int){
      def +(that:Pos):Pos = Pos(x + that.x, y + that.y)
    }

    val dir = List(Pos(-1,0), Pos(1,0), Pos(0,1),Pos(0,-1))
    def shortestPath(grid: Array[Array[Int]], K: Int): Int = {
      var ans = Int.MaxValue
      val n = grid.length
      val m = grid(0).length
      val target = Pos(n-1, m-1)
      val start = Pos(0,0)
      val mem = scala.collection.mutable.HashMap[Pos,Int]()
      val mem2 = scala.collection.mutable.HashMap[Pos,Int]()
      def inBound(p:Pos):Boolean = {
        p.x>= 0 &&
          p.y>= 0 &&
          p.x < n &&
          p.y < m
      }

      def next(p:Pos):List[Pos] = {
        dir.map{x => x + p} filter inBound
      }

      def isObs(p:Pos):Boolean = grid(p.x)(p.y) == 1


      def bfs(cur:Pos,k:Int, path:List[Pos]):Unit = {
        if(cur == target) ans = ans min path.length else {
          if(mem.contains(cur) && mem(cur) >= k && mem2(cur) <= path.length) {
            // cut
          }
          else {
            mem.put(cur, k)
            mem2.put(cur, path.length)
            next(cur) filterNot path.contains foreach {p =>
              if(isObs(p)) {
                if(k > 0) bfs(p, k-1, cur::path)
              } else bfs(p, k , cur::path)
            }
          }
        }
      }

      if(K >= m + n-2) m+n-2 else {
        bfs(start, K, Nil)
        if(ans == Int.MaxValue) -1 else ans
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(1,1,3,2,4,3,2),
      Array(1,1,3,2,4,3,2),
      Array(1,1,3,2,4,3,2)
    )
    println(p3.maxSideLength(arr,4))
  }
}
