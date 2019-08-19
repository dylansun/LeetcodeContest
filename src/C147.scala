/**
  * Created by lilisun on 7/28/19.
  */
object C147 {
  object P1 {
    def tribonacci(n: Int): Int = {
      def f(a:Int, b:Int, c:Int, n:Int):Int = {
        if (n == 0) a
        else f(b, c, a+b+c, n-1)
      }

      f(0,1,1,n)
    }
  }
  object P2 {
    // ["abcde",
    //  "fghij",
    //  "klmno",
    //  "pqrst",
    //  "uvwxy",
    //  "z"]
    def alphabetBoardPath(target: String): String = {
      def f(s:Char, t:Char):String = (pos(s), pos(t)) match {
        case ((x1,y1), (x2, y2)) =>
          var ans = ""
          // go left
          if(y1 > y2) ans += "L" * (y1 - y2)
          else if(y1 < y2) ans += "R" * (y2 - y1)
          else {}

          //
          if(x1 > x2) ans += "U" *(x1 - x2)
          else if(x1 < x2) ans += "D" *(x2 - x1)
          else {}

          if(s == 'z') ans.reverse + "!" else ans + "!"

      }

      def pos(ch:Char):(Int, Int) = {
        ((ch - 'a') / 5, (ch - 'a') % 5)
      }
      //('a' to 'z') foreach {ch => print(pos(ch))}
      ("a" + target) zip target map {case (s, t) => f(s, t)} reduce (_+_)
    }
  }
  object P3 {
    def largest1BorderedSquare(grid: Array[Array[Int]]): Int = {
      // ___________
      // |         |
      // |         |
      // ___________

      def f(i:Int, j:Int, k:Int):Boolean = {
        for{x <- j until j + k if grid(i)(x) == 0} return false
        for{x <- j until j + k if grid(i+k-1)(x) == 0} return false
        for{y <- i until i + k if grid(y)(j) == 0} return false
        for{y <- i until i + k if grid(y)(j+k-1) == 0} return false
        true
      }
      val n = grid.length
      val m = grid(0).length

      for{
        k <- (n min m) to 1 by -1
        i <- grid.indices
        j <- grid(i).indices
        x = i+k-1
        y = j+k-1
        if x < n && y < m
      }{
        if(f(i,j,k)) return k*k
      }
      0
    }
  }
  object P4 {
    def stoneGameII(A: Array[Int]): Int = {
      val n = A.length
      val dp = Array.fill(51, n+1)(0)
      def gain(i:Int):Int = A.slice(i-1,A.length).sum
      for{
        i <- n to 1 by -1
        m <- 1 to 50
      }{
        if(2*m >= (n-i+1))
          dp(m)(i) = A.slice(i-1,n).sum
        else for{x <- 1 to 2*m}
          dp(m)(i) = dp(m)(i) max (gain(i) - dp((x max m) min 50)(i+x))
      }
      dp(1)(1)
    }
  }
}
