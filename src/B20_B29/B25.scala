object B25 {
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies map {x => (x + extraCandies) >= candies.max}
  }
  def maxDiff(num: Int): Int = {
    val xs = for {
      x <- 0 to 9
      y <- 0 to 9
      n = num.toString.replaceAll(x.toString, y.toString)
      if n(0) != '0'
    } yield n.toInt

    (for {x <- xs; y <- xs} yield math.abs(x-y).toInt).max
  }
  def checkIfCanBreak(s1: String, s2: String): Boolean = {
    ((s1.sorted zip s2.sorted) forall {case (a,b) => a>=b}) || ((s2.sorted zip s1.sorted) forall {case (a,b) => a>=b})
  }
  def numberWays(hats: List[List[Int]]): Int = {
    val m = hats.flatten.distinct.length
    val n = hats.length
    val hatsID = hats.flatten.distinct.toArray
    val dp = Array.fill(1 << n)(0)
    dp(0) = 1
    for {i <- 0 until m; state <- (1 << n)-1 to 1 by -1}{
      dp(state) = SafeCal.add(dp(state), SafeCal.sum(for{
        k <- 0 until n
        if Math.hasBitK(state, k)
        if hats(k).contains(hatsID(i))
      } yield dp(state - (1 << k))))
    }
    dp.last
  }

  def test1():Unit = {
    println(kidsWithCandies(Array(4,2,1,1,2),1).toList)
    println(kidsWithCandies(Array(12,1,12), 10).toList)
  }
  def test2():Unit = {
    for{x <- List(9,123456,10000,9288)}
    println(maxDiff(x))
  }
  def test3():Unit = {
    println(checkIfCanBreak( "abc",  "xya"))

    println(checkIfCanBreak("abe", "acd"))
    println(checkIfCanBreak("leetcodee", "interview"))
  }
  def test4():Unit = {
    val c0 = List(List(3,5), List(4,5), List(5))
    println(numberWays(c0))
      val c1 = List(List(1,2,3),List(2,3,5,6),List(1,3,7,9),List(1,8,9),List(2,5,7))
    println(numberWays(c1))
    val c2 = List(List(1,2,3,4),List(1,2,3,4),List(1,2,3,4),List(1,2,3,4))
    println(numberWays(c2))
  }
  def main(args: Array[String]): Unit = {
    test4()

  }

}
