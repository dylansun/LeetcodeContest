package B10_19

/**
  * Created by lilisun on 1/29/20.
  */
object B15 {

  object P1 {
    def findSpecialInteger(arr: Array[Int]): Int = {
      val A = Array.fill(100001)(0)
      arr foreach {i => A(i)+=1}
      A.zipWithIndex.sortBy{case (x,i) => x}.reverse.head._2
    }
  }

  object P2 {
    def removeCoveredIntervals(intervals: Array[Array[Int]]): Int = {
      val l = intervals.sortBy{case Array(a,b) => (a,b)}.toList
      def f(l:List[Array[Int]], acc:Int = 0):Int = l match {
        case Array(a,b)::Array(c,d)::t =>
          if(a<=c && b >= d) f(Array(a,b)::t, acc + 1)
          else f(Array(c,d)::t, acc)
        case _ => acc
      }
      l.length - f(l)
    }
  }

  class CombinationIterator(str: String, m: Int) {

    def f(str:String, n:Int):List[String] = {
      if(str.length == n) List(str)
      else if(str.length < n) Nil
      else if(n == 1) str.toList.map{x => x.toString}
      else {
        f(str.tail, n-1).map{x => str.head + x}  ++ f(str.tail, n )
      }
    }

    var l = f(str, m)
    def next(): String = {
      val ans = l.head
      l = l.tail
      ans
    }

    def hasNext(): Boolean = l.nonEmpty
  }

  object P4 {
    def minFallingPathSum(A: Array[Array[Int]]): Int = {
      val n = A.length
      val dp = Array.fill(n,n)(0)
      for{
        i <- 0 until n
        j <- 0 until n
      } if(i == 0) dp(i)(j) = A(0)(j) else
        dp(i)(j) = A(i)(j) + (for{k <- 0 until n if k != j} yield dp(i-1)(k)).min
      dp.last.min
    }
  }
}
