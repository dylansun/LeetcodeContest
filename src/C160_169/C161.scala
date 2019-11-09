package C160_169

/**
  * Created by lilisun on 11/10/19.
  */
object C161 {
  object P1 {
    def minimumSwap(s1: String, s2: String): Int = {
      def g(x:Int, y:Int):Int = (x % 2, y % 2) match {
        case (1,1) => 2 + x / 2 + y / 2
        case (0,0) => x / 2 + y / 2
        case (_,_) => -1
      }
      def f(l:List[(Char, Char)], xy:Int, yx:Int):Int = l match {
        case Nil => g(xy, yx)
        case h::t => h match {
          case ('x','y') => f(t, xy+1, yx)
          case ('y','x') => f(t, xy, yx+1)
          case _ => f(t,xy,yx)
        }
      }
      f((s1 zip s2).toList, 0, 0)
    }
  }
  object P2 {
    def numberOfSubarrays(A: Array[Int], k: Int): Int = {
      def f(l:List[Int], acc:List[Int] = Nil, count:Int = 0):List[Int] = l match {
        case Nil => count::acc
        case h::t => h % 2 match {
          case 0 => f(t, acc, count+1)
          case 1 => f(t, count::acc, 0)
        }
      }
      def g(A:Array[Int], k:Int)(i:Int, acc:Int):Int =
        if(i+k >= A.length) acc else {
          g(A,k)(i+1, acc + (A(i+k)+1) * (A(i)+1))
        }
      g(f(A.toList).toArray, k)(0,0)
    }
  }
  object P3 {
    def minRemoveToMakeValid(s: String): String = {
      var l = List.empty[Int]
      var r = List.empty[Int]
      s.indices foreach {i => s(i) match {
        case '(' => l = i::l
        case ')' => l match {
          case Nil => r = i::r
          case h::t => l = t
        }
        case _ => {}
      }
      }
      val lr = (l++r).toSet
      s.indices filterNot lr.contains map s mkString
    }
  }

  object P4 {
    def gcd(x:Int, y:Int):Int = if(x % y == 0) y else gcd(y, x % y)
    def isGoodArray(A: Array[Int]): Boolean = {
      def f(l:List[Int] , acc:Int):Int = l match {
        case Nil => acc
        case h::t => f(t, gcd(acc, h))
      }
      f(A.toList.tail, A(0)) == 1
    }
  }

}
