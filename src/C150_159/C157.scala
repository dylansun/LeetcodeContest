package C150_159

/**
  * Created by lilisun on 10/6/19.
  */
object C157 {
  object P1 {
    def minCostToMoveChips(A: Array[Int]): Int = {
      val dp = Array(0,0)
      A.map{x => x % 2} foreach {x => dp(x) += 1}
      dp.min
    }
  }
  object P2 {
    def longestSubsequence(A: Array[Int], d: Int): Int = {
      val fre = scala.collection.mutable.HashMap[Int, Int]()
      A foreach {x => fre.put(x, (fre.getOrElse(x-d, 0) + 1) max fre.getOrElse(x, 0))}
      fre.values.max
    }
  }
  object P3 {
    case class Pos(x:Int, y:Int){
      def +(that:Pos):Pos = Pos(x + that.x, y + that.y)
    }
    def getMaximumGold(grid: Array[Array[Int]]): Int = {
      val m = grid.length
      val n = grid(0).length
      var max_gain = 0
      val dir = List(Pos(0,1), Pos(0,-1), Pos(1,0), Pos(-1, 0))
      def inBound(pos:Pos):Boolean = pos match {
        case Pos(x, y) => x >= 0 && y >= 0 && x < m && y < n
      }
      def get(pos:Pos):Int = grid(pos.x)(pos.y)
      def dfs(l:List[Pos], gain:Int):Unit = l match {
        case h::t => if(get(h) != 0){
            val new_gain = gain + get(h)
            max_gain = max_gain max new_gain
            dir.map(x => x + h) filter inBound filterNot t.contains foreach {hh => dfs(hh::h::t, new_gain)}
          }
      }

      for{
        i <- 0 until m
        j <- 0 until n
      }
        dfs(List(Pos(i,j)), 0)

      max_gain
    }
  }
  object P4 {
    def countVowelPermutation(n: Int): Int = {
      val mod = (1e9 + 7).toInt
      val dp = Array.fill(n+1, 5)(BigInt(0)) // a, e, i, o, u
      for{i <- 0 to 4} dp(1)(i) = 1
      for{k <- 2 to n}{
        dp(k)(0) = (dp(k-1)(1) + dp(k-1)(4) + dp(k-1)(2)) % mod
        dp(k)(1) = (dp(k-1)(0) + dp(k-1)(2)) % mod
        dp(k)(2) = (dp(k-1)(1) + dp(k-1)(3)) % mod
        dp(k)(3) = dp(k-1)(2) % mod
        dp(k)(4) = (dp(k-1)(2) + dp(k-1)(3)) % mod
      }

      (dp(n).sum % mod).toInt
    }
  }
}
