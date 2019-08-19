/**
  * Created by lilisun on 6/23/19.
  */
object C142 {
  object Solution {
    def sampleStats(A: Array[Int]): Array[Double] = {
      def min_A(A:Array[Int]):Int = {
        for(i <- A.indices if A(i) > 0) return i
        -1
      }
      def max_A(A:Array[Int]):Int = {
        for(i <- A.indices.reverse if A(i) > 0) return i
        -1
      }
      def mean_A(A:Array[Int]):Double = {
        var sum_A = BigInt(0)
        for(i <- A.indices) {
          sum_A += BigInt(i) * BigInt(A(i))

        }
        val p = sum_A
        val q = A.sum
        if(p > q){
          (p / q).toDouble + (p % q).toDouble / q.toDouble
        }else{
          p.toDouble / q.toDouble
        }
      }

      def median_A (A:Array[Int]):Double = {
        0
      }

      def most_A (A:Array[Int]):Int = {
        A.zipWithIndex.sortBy{case (x, y) => - y}.head._1
      }

      Array(min_A(A), max_A(A), mean_A(A), median_A(A), most_A(A))
    }
  }

  object Solution2 {
    def carPooling(A: Array[Array[Int]], c: Int): Boolean = {
      def f(l:List[Array[Int]], acc:List[Array[Int]]):Boolean = l match {
        case Nil => true
        case Array(x, y, z)::t =>
          val nl = Array(x, y,z)::acc.filter{case Array(a, b, c) => c <= y}
          if(nl.map{case Array(x, y,z) => x}.sum > c) false
          else f(t, nl)
      }
      f(A.toList.sortBy{case Array( x, y, z) => y}, Nil)
    }
  }


  def main(args: Array[String]): Unit = {
    println(Solution2.carPooling(Array(Array(2,1,5), Array(3,5,7)), 3))
  }
}
