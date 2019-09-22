
object B8 {
  object P2 {
    def beforeAndAfterPuzzles(phrases: Array[String]): Array[String] = {
      val A = phrases.sorted
      (for {
        i <- A.indices
        j <- i+1 until A.length

        xs = A(i) split ' '
        ys = A(j) split ' '
        if xs.last == ys.head
      } yield {
        (xs ++ ys.tail).mkString(" ")
      }).toArray
    }
  }

  def test_P2():Unit = {
    val phrase = Array("a","b","a")
    P2.beforeAndAfterPuzzles(phrase) foreach println
  }

  object P3 {
    def shortestDistanceColor(colors: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
      val n = colors.length
      val dp = Array.fill(n, 3)(0)
      def forward(i: Int, j: Int, seen: Set[Int]): Unit = ???
      colors
    }
  }

  def test_P3():Unit = {

  }


  def main(args: Array[String]): Unit = {

  }
}
