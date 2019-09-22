import scala.collection.mutable.TreeSet
object C153 {
  object P3 {
    def maximumSum(A: Array[Int]): Int = {
      val n = A.length
      val f = Array.fill(n)(0)
      val g = Array.fill(n)(0)
      f(0) = A(0)
      g(0) = -20001
      1 until n foreach { i =>
        f(i) = (f(i-1)+A(i)) max A(i)
        g(i) = (g(i-1) + A(i)) max f(i-1)
      }
      var res = Int.MinValue
      f.indices foreach {i => res = res max f(i) max g(i)}
      res
    }
  }

  object P4 {
    def makeArrayIncreasing(A: Array[Int], B: Array[Int]): Int = {
      if(A.isEmpty) return -1
      if(A.length == 1) return 0
      val n = A.length
      val dp = Array.fill(n+1,n+1)(Int.MaxValue)
      dp(0)(0) = Int.MinValue
      val ts = TreeSet.empty[Int]
      B foreach {i => ts.add(i)}

      for{
        j <- 1 to n
        i <- 0 to j
      }{
        if (A(j - 1) > dp(i)(j - 1)) {
          dp(i)(j) = A(j - 1)
        }
        if (i > 0 && ts.exists(x => x > dp(i - 1)(j - 1))) {
          dp(i)(j) =dp(i)(j) min ts.find(x => x > dp(i-1)(j-1)).get
        }
        if (j == dp.length - 1 && dp(i)(j) != Int.MaxValue) return i
      }
      -1
    }
  }

  def main(args: Array[String]): Unit = {
    val ts = TreeSet.empty[Int]
    ts.
    val A = Array(1,4,5,6,3,2,0)
    A.foreach(x => ts.add(x))
    println(ts)
    println(ts.find(x => x > 8))
  }
}
