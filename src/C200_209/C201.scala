package C200_209

/**
  * Created by lilisun on 8/13/20.
  */
object C201 {
  object P1{
    def findKthPositive(arr: Array[Int], k: Int): Int = {
      ((1 to 2000).toArray filterNot arr.contains ).array(k-1)

    }
  }
  object P2{
    def canConvertString(s: String, t: String, k: Int): Boolean = {
      val last = scala.collection.mutable.HashMap[Int, Int]()
      last.put(0,0)
      def dist(a:Char, b:Char):Int = (b - a + 26 ) % 26
      s zip t map {case (a, b) => dist(a, b)} filterNot {_==0} foreach { x =>
        println(x)
        if(last.contains(x)) last(x) += 26 else last.put(x, x)
      }
      s.length == t.length && last.values.max <= k
    }
  }
  object P3{
    def minInsertions(s: String): Int = solve(s.toList, 0, 0, 0)
    def solve(l:List[Char], L:Int, R:Int, acc:Int): Int = l match {
      case Nil => acc + 2*L - R
      case '('::t => (L, R) match {
        case (0,1) => solve(t, 1, 0, acc + 2)
        case (_,0) => solve(t, L+1, 0, acc)
        case (_,1) => solve(t, L, 0, acc + 1)
        case (_,_) => -1
      }
      case ')'::t => (L, R) match {
        case (0, 0) => solve(t, 1, 1, acc + 1)
        case (_, 1) => solve(t, L-1, 0, acc )
        case (_, 0) => solve(t, L, 1, acc )
        case (_,_) => -1
      }
      case _ => -1
    }
  }

  object P4{
    def longestAwesome(s: String): Int = {
      val dp = Array.fill( 1 << 10) ( s.length)
      dp(0) = -1
      def sign(state:Int, j:Int):Int = if(state & (1 << j) == 0) 1 else -1
      def solve(l:List[Int], i:Int, state:Int, acc:Int):Int = l match {
        case Nil => acc
        case h::t =>
          val nstate = state + sign(state, h) * ( 1 << h)
          if (nstate == 0) solve(t, i+1, 0, i+1) else {
            val seq = for{j <- 0 to 9} yield i - dp(nstate + sign(nstate, j) * (1 << j))
            dp(nstate) = dp(nstate) min i
            solve(t, i+1, nstate, acc max seq.max)
          }
      }
      solve(s.toList.map(_-'0'), 0, 0, 0)
    }
  }
}
