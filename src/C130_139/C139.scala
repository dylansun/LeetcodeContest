object C139 {
  object P1 {
    def gcdOfStrings(s1: String, s2: String): String = {
      val n = gcd(s1.length, s2.length)
      //println(n)
      for{
        k <- n to 1 by -1
        if n % k == 0
        n1 = s1.length / k
        n2 = s2.length / k
        if s1.slice(0,k) == s2.slice(0,k)
        if (0 until n1).map(x => s1.slice(x*k, (x+1)*k)).toSet.size == 1
        if (0 until n2).map(x => s2.slice(x*k, (x+1)*k)).toSet.size == 1
      } return s1.slice(0, k)
      ""
    }

    def gcd(x:Int, y:Int):Int = {
      if(x % y == 0) y
      else gcd(y, x % y)
    }
  }
  object P2 {
    def maxEqualRowsAfterFlips(matrix: Array[Array[Int]]): Int = {
      matrix.map(_.toList).map{case h::t => t.map(_ ^ h)}.groupBy(x => x).values.map(_.length).max
    }
  }
  object P3 {
    def addNegabinary(A: Array[Int], B: Array[Int]): Array[Int] = {
      def addBit(x:Int, y:Int, carry:Int):Int = {
        x + y + carry match {
          case 0 => 0
          case 1 => 1
          case -1 => 1
          case 2 => 0
          case 3 => 1
        }
      }
      def calCarry(x:Int, y:Int, carry:Int):Int = {
        x + y + carry match {
          case 0 => 0
          case 1 => 0
          case -1 => 1
          case 2 => -1
          case 3 => -1
        }
      }
      def f(l1:List[Int], l2:List[Int], carry :Int = 0, acc:List[Int]):List[Int] = {
        (l1, l2) match {
          case (h1::t1, h2::t2) => f(t1, t2, calCarry(h1,h2,carry),addBit(h1, h2, carry)::acc)
          case (h1::t1, Nil) => f(t1,Nil, calCarry(h1, 0, carry), addBit(h1, 0, carry)::acc)
          case (Nil, h1::t1) => f(t1,Nil, calCarry(h1, 0, carry), addBit(h1, 0, carry)::acc)
          case (Nil, Nil) => carry match {
            case 0 => acc
            case 1 => 1::acc
            case -1 => 1::1::acc
          }
        }
      }
      def rmzero(l:List[Int]):List[Int] = l match {
        case 0::h::t => rmzero(h::t)
        case _ => l
      }
      rmzero(f(A.reverse.toList, B.reverse.toList, 0, Nil)).toArray
    }
  }

}
