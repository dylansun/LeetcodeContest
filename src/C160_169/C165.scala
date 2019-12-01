package C160_169

/**
  * Created by lilisun on 12/2/19.
  */
object C165 {
  object P1 {
    // 0 1 2
    // 3 4 5
    // 6 7 8
    def g(a:Int, b:Int, c:Int):Int = 0 | (1 << a) | (1 << b) | (1 << c)
    val pattern = Array(g(0,1,2), g(3,4,5), g(6,7,8),
      g(0,3,6), g(1,4,7), g(2,5,8),
      g(0,4,8), g(2,4,6))
    def f(x:Int, y:Int):Int = x * 3 + y
    def isWin(x:Int):Boolean = pattern.exists{p => (x & p) == p}
    def tictactoe(moves: Array[Array[Int]]): String = {
      if(moves.length < 5) "Pending" else
        moves
          .zipWithIndex
          .groupBy{case (x, i) => i % 2}
          .toList
          .sortBy{case (k,v) => k}
          .map{case (k,v) => v.foldLeft(0){(res, elem) => elem match {
            case (Array(x,y), i) => res | (1 << f(x,y))
          }}} match {
          case a::b::Nil =>
            if(isWin(a)) "A"
            else if(isWin(b)) "B"
            else if(moves.length == 9) "Draw"
            else "Pending"
        }
    }
  }

  object P2 {
    // a, b
    // x, y
    // 4x + 2y = a
    // x + y = b
    // x = (a - 2b)/ 2 = a / 2- b
    // y = b - x = 2*b - a/2
    def p(a:Int, b:Int):Boolean = {
      a >= 2*b &&
        (a % 2 == 0) &&
        (2*b >= a / 2)
    }
    def numOfBurgers(a: Int, b: Int): List[Int] = {
      if(p(a,b)) List(a / 2 - b, 2*b - a / 2) else Nil
    }
  }

  /*
     优化: 如何快速计算最大正方形的边长?
   */
  object P3_TLE {
    /*
       ex:
       [0,1,1,1],
       [1,1,1,1],
       [0,1,1,1]

       dp matrix:
       [0,3,2,1],
       [1,2,2,1],
       [0,1,1,1]

       ex:
       [1,0,1],
       [1,1,0],
       [1,1,0]

       dp matrix:
       [1,0,1],
       [2,1,0],
       [1,1,0]
     */
    def countSquares(A: Array[Array[Int]]): Int = {
      val zero = (for{
        i <- A.indices
        j <- A(i).indices
        if A(i)(j) == 0
      } yield (i,j)).toList

      // maximum square
      def get(x:Int, y:Int):Int = {
        (A.length - x) min (A(0).length - y)
      }
      def dist(a:Int, b:Int, x:Int, y:Int):Int = {
        (if(a == x) b - y
        else if(b == y) a - x
        else (a-x) max (b - y)) min (get(x,y))
      }
      // find the maxium square
      def find(x:Int, y:Int):Int = dofind(zero, x,y, get(x,y))
      def dofind(l:List[(Int,Int)], x:Int, y:Int, acc:Int):Int = l match {
        case Nil => acc
        case (a,b)::t =>
          if(a >= x && b >= y) dofind(t, x,y, acc min dist(a,b,x,y))
          else dofind(t, x, y, acc)
      }

      val dp = Array.fill(A.length, A(0).length)(0)
      for{
        i <- A.indices
        j <- A(i).indices
        if A(i)(j) == 1
      } dp(i)(j) = find(i,j)
      dp.flatten.sum
    }
  }

  object P4 {
    /*
      abcdef, cost = 3
      abcdefg, cost = 3
      abcdefa, cost = 2,

      f(i,j) = f(i+1, j-1) + cost(i,j)

      DP:
        假设字符串长度为n, 拆分为k
        f(n, k)
        f(n, k-1)
        solve k = 1:
          f(n, 1) == f(0, n-1, 1)
          initial: f(i,i,1) = 0, length = 1
          increase length from 2 to n, d <- 1 to n-1
          f(i,i+d, 1) = f(i+1,i+d-1,1) + cost(i,i+d)
          // i+d-1 >= i+1 => d >= 2
          // if d == 1, f(i,i+1,1) = cost(i,i+1,1)

        solve k = 2:
          f(0,n-1, 2) = min {f(0,p,1) + f(p+1,n-1,1):for p in 0 to n-1}

        solve k:
          f(0, n-1, k) = min {f(0, p, 1) + f(p+1, n-1,k-1): for p in 0 to n -1}

    */
    def palindromePartition(s: String, K: Int): Int = {
      val n = s.length
      val dp = Array.fill(n,n,K+1)(0)
      def cost(x:Int, y:Int):Int = if(s(x) == s(y)) 0 else 1
      // intial k = 1:
      for{
        d <- 1 until n
        i <- 0 until n
        if i + d < n
      }{
        if(d == 1) dp(i)(i+1)(1) = cost(i,i+1)
        else dp(i)(i+d)(1) = dp(i+1)(i+d-1)(1) + cost(i,i+d)
      }

      for{k <- 2 to K}{
        for{
          d <- k until n
          i <- 0 until n
          j = i+d
          if j < n
        }
        dp(i)(j)(k) = (for{p <- i until j} yield dp(i)(p)(1) + dp(p+1)(j)(k-1)).min
      }

      dp(0)(n-1)(K)
    }
  }

  def test4():Unit = {
    val s = Array("tcymekt", "abc", "aabbc", "leetcode")
    val k = Array(4,2,3,8)
    (s zip k) foreach { case (a, b) =>
      println(P4.palindromePartition(a, b))
    }
  }
  def main(args: Array[String]): Unit = {
    test4()
  }
}
