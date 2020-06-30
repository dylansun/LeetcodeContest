object B29 {
  object P1 {
    def average(salary: Array[Int]): Double = {
      mean(salary.sorted.slice(1, salary.length -1))
    }

    def mean(A:Array[Int]):Double = A.sum.toDouble / A.length
  }
  object P2 {
    def kthFactor(n: Int, k: Int): Int = {
      val l = (1 to n).toList.filter(x =>n % x==0)
      if(l.length < k) -1 else l(k-1)
    }
  }
  object P3 {
    def longestSubarray(nums: Array[Int]): Int = {
      val n = nums.length
      val l = Array.fill(n)(0)
      val r = Array.fill(n)(0)
      for{
        i <- l.indices
        if i -1 >= 0
        if nums(i-1) == 1
      } l(i) = 1 + l(i-1)

      for{
        i <- r.indices.reverse
        if i+1 < r.length
        if nums(i+1) == 1
      } r(i) = 1 + r(i+1)

      (for{
        i <- 0 until n
      } yield l(i) + r(i)).max

    }
  }
  def main(args: Array[String]): Unit = {

  }
}
