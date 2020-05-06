import scala.collection.mutable.{HashMap => HM, ArrayBuffer => AB}
object C186 {
  def maxScore(s: String): Int = {
    val n = s.length
    if(n==0) 0 else
      (for{i <- 1 until n} yield
        s.slice(0,i).filter(_=='0').length +
          s.slice(i,n).filter(_=='1').length).max
  }
  def maxScore(A: Array[Int], k: Int): Int = {
    val cache = new Cache[Int, Int]()
    def f(x:Int):Int = if(x < 0) 0 else A(x) + cache.cache(f, x-1)
    def rsum(x:Int,y:Int):Int = f(y) - f(x-1)
    A.sum - (for{i <- A.indices;p = i+A.length - k-1 if p < A.length } yield rsum(i, p)).min
  }

  // fail at test case #53
  def findDiagonalOrder(nums:List[List[Int]]): Array[Int] = {
    val mem = HM[Int,AB[Int]]()
    for{i <- nums.indices.reverse;j <- nums(i).indices}{
      if(mem.contains(i+j)) mem(i+j).append(nums(i)(j))
      else mem.put(i+j, AB(nums(i)(j)))
    }
    mem.keySet.toList.sorted map mem reduce (_++_) toArray
  }


  def main(args: Array[String]): Unit = {
    val l = List(List(1,2,3), List(8), List(2,3,312,313))
    val ab = AB(10,12)
    ab.append(11)
    println(ab)
    println(findDiagonalOrder(l).toList)
    l forall (_==Nil)
  }
}
