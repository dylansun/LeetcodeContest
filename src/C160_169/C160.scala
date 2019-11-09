package C160_169

/**
  * Created by lilisun on 11/4/19.
  */
object C160 {

  // P1 not support scala

  // Generate Gray Code
  object P2 {
    // 0 1
    // 00 01 10 11 => 00 01 11 10
    // 000 001 011 010 , 100 101 111 110 => 000 001 011 010 , {100 101 111 110 } reverse
    def circularPermutation(n: Int, start: Int): List[Int] = {
      def f(n:Int,d:Int, l:List[Int]):List[Int] = n match {
        case 1 => l
        case _ =>
          f(n-1,d+1 ,l ++ (l map {x => x | (1 << d)}).reverse)
      }
      def trans(l:List[Int], acc:List[Int], target:Int):List[Int] = l match {
        case Nil => acc
        case h::t =>
          if(h == target) (h::t) ++ acc.reverse
          else trans(t, h::acc, target)
      }
      trans(f(n,1, List(0,1)),Nil,  start)
    }
  }


  object P3 {
    def f(str:String):Int = f(str.toList, 0)
    def f(l:List[Char], acc:Int = 0):Int = l match {
      case Nil => acc
      case h::t => f(t, acc | (1 << (h - 'a')))
    }
    def g(x:Int, acc :Int = 0):Int = x match {
      case 0 => acc
      case _ => g(x >> 1, acc + (x & 1))
    }
    def maxLength(l: List[String]): Int = {
      val A = l filter {str => str.length == str.distinct.length} map f
      def solve(l:List[Int], set:Set[Int]):Set[Int] = l match {
        case Nil => set
        case h::t =>
          val new_elems = for{
            x <- set
            if (x & h) == 0
          } yield x | h

          solve(t, set ++ new_elems)
      }

      solve(A, Set(0)) map {x => g(x, 0)} max
    }
  }
  /*
      class Solution {
        public int tilingRectangle(int n, int m) {
            int[][] dp=new int[15][15];

            for(int i=1;i<=n;i++)
              for(int j=1;j<=m;j++) {
                dp[i][j]=i==j?1:i*j;
                for(int p=1;p<i;p++)
                  dp[i][j]=Math.min(dp[i][j], dp[p][j]+dp[i-p][j]);
                for(int p=1;p<j;p++)
                  dp[i][j]=Math.min(dp[i][j], dp[i][p]+dp[i][j-p]);
                for(int x=2;x<i;x++)
                  for(int y=2;y<j;y++)
                    dp[i][j]=Math.min(dp[i][j], dp[x-1][y]+dp[x][j-y]+dp[i-x+1][y-1]+dp[i-x][j-y+1]+1);
              }
            return dp[n][m];
        }
    }

   */
  object P4{
    def tilingRectangle(n: Int, m: Int): Int = {
      val dp = Array.fill(15,15)(1000)
      for{
        i <- 1 to n
        j <- 1 to m
      }{
        dp(i)(j) = if(i == j) 1 else i * j
        for{p <- 1 until i} dp(i)(j) = dp(i)(j) min (dp(p)(j) + dp(i-p)(j))
        for{p <- 1 until j} dp(i)(j) = dp(i)(j) min (dp(i)(p) + dp(i)(j-p))
        for{
          x <- 2 until i
          y <- 2 until j
        } dp(i)(j) = dp(i)(j) min (dp(x-1)(y) + dp(x)(j-y) + dp(i-x+1)(y-1) + dp(i-x)(j-y+1) + 1)
      }
      dp(n)(m)
    }
  }
  def main(args: Array[String]): Unit = {
    println(1 << 31)
    println(1 << 20)
    println(1 << 26)
    println(1 << 30)
  }
}
