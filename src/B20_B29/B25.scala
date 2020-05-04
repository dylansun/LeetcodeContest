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
  def main(args: Array[String]): Unit = {
    test3()

  }

}
