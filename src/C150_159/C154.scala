object C154 {
  object P1 {
    def maxNumberOfBalloons(text: String): Int = {
      val cnt = Array.fill(26)(0)
      text foreach {ch => cnt(ch - 'a') += 1}
      // balloon
      Array('b','a','l','o','n') map {
        case 'l' => cnt('l' - 'a') / 2
        case 'o' => cnt('o' - 'a') / 2
        case ch  => cnt(ch - 'a')
      } min
    }
  }

  object P2 {
    def reverseParentheses(s: String): String = {
      def f(input:List[Char], stack:List[Char]):List[Char] = input match {
        case Nil => stack
        case ')'::t => f(t, g(stack))
        case h::t => f(t, h::stack)
      }
      def g(l:List[Char], acc:List[Char] = Nil):List[Char] = l match {
        case Nil => acc
        case '('::t =>  acc ++ t
        case h::t => g(t, h::acc)
      }

      f(s.toList, Nil).mkString.reverse
    }
  }
  object P3 {
    val mod = 1000000007
    def kConcatenationMaxSum(A: Array[Int], k: Int): Int = {
      val n = A.length
      val prefix = Array.fill(n+1)(BigInt(0))
      val dp = Array.fill(n)(BigInt(0))
      A.indices foreach {i =>
        prefix(i+1) = prefix(i) + A(i)
        dp(i) = ((if(i == 0) BigInt(0) else dp(i-1)) + A(i)) max BigInt(0)
      }
      if(k == 1) dp.max.toInt
      else {
        val a = prefix(n) - prefix.min + prefix.max
        val b = (k-2) * (prefix(n) max 0)
        (((a + b) max dp.max) % mod).toInt
      }
    }
  }
}
