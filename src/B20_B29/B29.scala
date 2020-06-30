import scala.collection.mutable.{HashMap => HM}
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

=======
    def kthFactor(n: Int, k: Int): Int =
      (1 to n).toList.filter(x =>n % x==0).drop(k-1).headOption.getOrElse(-1)
  }
  object P3 {
    def longestSubarray(nums: Array[Int], K: Int = 1): Int = {
      val dp = Array.fill(nums.length, K + 1)(0)
      def get(i: Int, k: Int): Int = if (i < 0 || k < 0) 0 else dp(i)(k)
      for {
        i <- nums.indices
        k <- 0 to K
        p = nums(i) == 1
      } dp(i)(k) = if (p) 1 + get(i - 1, k) else get(i - 1, k - 1)
      dp.flatten.max min (nums.length - K)
    }
  }
  /**
    拓扑排序+贪心可以过大多数case，但是不是正确的解法。
    */
  object P4_Greedy{
    def minNumberOfSemesters(n: Int, dependencies: Array[Array[Int]], k: Int): Int = {
      val tp = new Topology[Int]()
      val seqs = dependencies.toList.map{case Array(a,b) => Edge[Int](a, b)}
      val inDegress = HM[Int, Int]()
      for{i <- 1 to n } inDegress.put(i, 0)
      tp.build_graph(seqs, inDegress) match {case (deg, g) =>
        tp.min_topology_sort_steps(deg, g,k )}
    }
  }

  object P4_DP{
    def minNumberOfSemesters(n: Int, dependencies: Array[Array[Int]], k: Int): Int = {
      val prereq = Array.fill(n)(0)
      val dp = Array.fill(1 << n)(1e9.toInt)
      dependencies foreach {case Array(x,y) => prereq(y-1) |= 1 << (x -1)}
      val set_prereq = Array.fill(1 << n)(0)
      val valid = Array.fill(1 << n)(false)
      for{
        mask <- 0 until 1 << n
        if count_one(mask) <= k
      }{
        for{
          b <- 0 until n
          if ((mask >> b) & 1) != 0
        } set_prereq(mask) |= prereq(b)
        valid(mask) = (set_prereq(mask) & mask) == 0
      }

      dp(0) = 0
      for{mask <- 0 until 1 << n}{
        var t = mask
        while(t>0){
          if(valid(t) &&((mask & set_prereq(t)) == set_prereq(t))) {
            dp(mask) = dp(mask) min (dp(mask ^ t) + 1)
          }
          t = (t-1)&mask
        }
      }
      dp.last
    }
    def count_one(x:Int):Int = if(x==0) 0 else (x & 1) + count_one(x >> 1)
  }
}
