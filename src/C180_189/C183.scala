object C183 {
  object P1 {
    def minSubsequence(A: Array[Int]): List[Int] = {
      val l = A.sorted.reverse.toList
      val s = l.sum
      def f(l:List[Int],cur:Int, acc:List[Int]):List[Int] = l match {
        case Nil => acc.reverse
        case h::t => if(cur+h>s/2) (h::acc).reverse else f(t, cur+h, h::acc)
      }
      f(l,0,Nil)
    }
  }
  object P2 {
    val p = BigInt(1)
    def numSteps(s: String): Int =  f(str2Int(s.toList))
    def f(x:BigInt, acc:Int = 0):Int = if(x==1) acc else {
      if(p == (x%2))f(1+x, acc+1) else  f(x/2, acc+1)
    }
    def str2Int(l:List[Char], acc:BigInt = BigInt(0)):BigInt = l match {
      case Nil => acc
      case h::t => str2Int(t,acc*2 + h - '0')
    }
  }

  object P3 {
    case class E(str:String, n:Int)
    def longestDiverseString(a: Int, b: Int, c: Int): String = {
      def f(l:List[E], pre:String, acc:String):String = {
        val l1 = l.filter(_.str == pre)
        val l2 = l.filter(x => x.str != pre && x.n != 0)
        val m = l.map{case E(_,i)=>i}.max
        l2.sortBy{case E(str, n) => -n} match {
          case Nil => acc
          case E(str, n)::t =>
            if(n>=2 && n == m) f ( E(str,n-2):: (t++l1), str,acc + str + str)
            else f(E(str,n-1)::(t ++ l1), str, acc+str)
        }
      }
      f(List(E("a",a), E("b",b), E("c",c)),"","")
    }
  }

  object P4 {
    def stoneGameIII(A: Array[Int]): String = {
      val cache2 = new Cache[Int, Int]()
      val c = Array.fill(A.length)(A.last)
      A.indices.reverse.tail foreach {i => c(i)=A(i)+c(i+1)}
      def dp(i:Int):Int =
        if(i >= A.length) 0
        else List(i+1,i+2,i+3).map{x => c(i) - cache2.cache(dp, x)}.max
      if(cache2.cache(dp,0) * 2== c.head ) "Tie"
      else if(cache2.cache(dp,0) * 2 >c.head) "Alice"
      else "Bob"

    }

  }
}
