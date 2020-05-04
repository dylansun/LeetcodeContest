import scala.language.postfixOps
import scala.collection.mutable
object B22 {

  object P1 {
    def findTheDistanceValue(arr1: Array[Int], arr2: Array[Int], d: Int): Int = {
      arr1 count {x => arr2 forall {y => Math.abs(x-y)>d}}
    }
  }

  object P2 {
    def maxNumberOfFamilies(n: Int, reservedSeats: Array[Array[Int]]): Int = {
      def f(l:List[Int]):Int = {
        if(p(2,9,l)) 0
        else if(p(2,5,l) || p(6,9,l) || p(4,7,l)) 1
        else 2
      }

      val tab = mutable.HashMap[Int,List[Int]]()
      reservedSeats foreach {case Array(i,j) => tab.put(i,j::tab.getOrElse(i,Nil))}
      2*n - (tab.values map f sum)
    }
    def p(start:Int, end:Int, l:List[Int]):Boolean = !(start to end exists l.contains)

  }

  object P3 {
    def getKth(lo: Int, hi: Int, k: Int): Int = {
      def weight(x:Int, acc:Int = 0):Int = if(x==1) acc else x % 2 match {
        case 1 => weight(3*x+1, acc+1)
        case 0 => weight(x / 2, acc+1)
      }
      (lo to hi).toArray.sortBy{x => (weight(x),x)}.array(k-1)
    }
  }

  /*
     找到n长的不含连续元素的子序列
     环形的解决:
     分类讨论, 不取第一个元素, 和 不取最后一个元素
   */
  object P4 {
    def maxSizeSlices(A: Array[Int]): Int = {
      def f(A:Array[Int]):Int = {
        val choose = (A.length + 1) / 3
        val dp = Array.fill(A.length, 1+choose)(0)
        for{i<-A.indices}
          for{j<-1 to choose}
            dp(i)(j) = (if(i-2>=0) dp(i-2)(j-1) else 0) + A(i) max (if(i-1>=0) dp(i-1)(j) else 0)
        dp.last.last
      }
      f(A.tail) max f(A.dropRight(1))
    }
  }

}
