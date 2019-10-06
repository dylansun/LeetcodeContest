package B10_19

/**
  * Created by lilisun on 10/5/19.
  */
object B10 {
  object Solution {
    def isValidPalindrome(s: String, k: Int): Boolean = if(k >= s.length - 1) true else
    {
      val mem = scala.collection.mutable.HashMap[Char, List[Int]]()
      s.indices foreach {i => mem.put(s(i), mem.getOrElse(s(i), Nil) ++ List(i))}
      for{ (k, v) <- mem} println(k, v)
      val pairs = (s.length - k) / 2
      def greedy(ch:Char, acc:Char, L:Int, R:Int):Char = if(ch > 'z')  acc else {
        println(s"greedy method: $ch, $L, $R")
        if(mem.contains(ch) && mem(ch).nonEmpty) {
          val l = mem(ch)
          if(l.last - l.head >= R - L) greedy((ch+1).toChar, ch, l.head, l.last)
          else greedy((ch+1).toChar, acc, L , R)
        }else greedy((ch+1).toChar, acc, L , R)
      }

      def solve(n:Int, L:Int, R:Int):Boolean = if(n== 0) true else {
        println(n, L, R)
        val ch = greedy('a', 'a', 0, -1)
        val l = mem(ch)
        println("found greedy: ",ch, l)
        if(l.length >= 2) {
          mem.put(ch, l.tail.dropRight(1))
          println(ch, l.head,l.last)
          solve(n-1, l.head, l.last)
        } else false
      }

      solve(pairs, 0, s.length)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.isValidPalindrome("abcdba",2))
  }
}
