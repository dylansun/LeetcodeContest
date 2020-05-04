import scala.collection.mutable.{ArrayBuffer=>MAB}
object C187 {
  def destCity(paths: List[List[String]]): String = {
    val src = paths.map(_.head)
    (paths map {case List(a,b) => b} filterNot src.contains).head
  }
  def kLengthApart(nums: Array[Int], k: Int): Boolean = {
    def f(l:List[Int]):Boolean = l match {
      case Nil => true
      case h::Nil => true
      case i::j::t => if(j-i>=k+1) f(l.tail) else false
    }
    f(nums.zipWithIndex.filter(_._1==1).map(_._2).toList)
  }
  def longestSubarray(nums: Array[Int], limit: Int): Int = {
    val A = MAB[Int]();val n = nums.length
    var l = 0; var ret = 0
    for {r <- 0 until n} {
      Bisect.insort(A, nums(r))
      while(A.last - A.head > limit) {
        A.remove(Bisect.find(A, nums(l)))
        l+=1
      }
      ret = ret max (r-l+1)
    }
    ret
  }
  def kthSmallest(mat: Array[Array[Int]], k: Int): Int = {
    var A = MAB[Int]()
    mat foreach {l =>
      if(A.isEmpty) l foreach {x => Bisect.insort(A, x)}
      else{
        val B = MAB[Int]()
        for{x <- l; y <- A} Bisect.insort(B, x+y)
        A = B
      }
      if(A.length > k) A = A.slice(0,k)
    }
    A.last
  }
}
