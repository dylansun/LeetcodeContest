package B1_9

/**
  * Created by lilisun on 6/29/19.
  */
object B3 {
  object Solution {
    // 0 1 2 3 4
    // 5 5+1,.. 5+4 i*m + j
    class DSU(N:Int){
      val parent = (0 until N).toArray
      def find(x:Int):Int = {
        val tmp = (if(x != parent(x)) find(parent(x)) else x)
        parent(x) = tmp
        tmp
      }
      def union(x:Int, y:Int):Unit = {parent(find(x)) = parent(find(y))}
    }
    def maximumMinimumPath(A: Array[Array[Int]]): Int = {
      val n = A.length
      val m = A(0).length
      if(n == 1 || m == 1) A.flatten.min
      var B = (for{
        i <- A.indices
        j <- A(i).indices
        if A(i)(j) < A(n-1)(m-1) && A(i)(j) < A(0)(0)
      } yield (i,j, A(i)(j))).toList.sortBy{case (i,j,x) => x}.reverse
      val dsu = new DSU(n * m)
      def inBound(p:(Int,Int)):Boolean = p match{
        case (x,y) => x >= 0 && x < n && y >= 0 && y < m
      }
      for{
        i <- 0 until n
        j <- 0 until m
        if ! B.contains((i,j,A(i)(j)))
      }{
        val nei = List((i-1,j),(i+1,j),(i,j-1),(i, j+1)) filter inBound
        nei.foreach{case (x,y) =>
          if(!B.contains((x,y,A(x)(y))))
            dsu.union(x*m + y, i*m+j)
        }
      }
      if(dsu.find(0) == dsu.find(n*m -1)) return A(0)(0) min A(n-1)(m-1)
      while(B.nonEmpty){
        val h = B.head
        B = B.tail
        h match {
          case (i,j, ans) =>
            val nei = List((i-1,j),(i+1,j),(i,j-1),(i, j+1)) filter inBound
            nei.foreach{
              case (x,y) =>
                if(!B.contains((x,y,A(x)(y))))
                  dsu.union(x*m + y, i*m+j)
            }
            if(dsu.find(0) == dsu.find(n*m -1)) return ans

        }
      }
      println("Error")
      return A.flatten.min
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
