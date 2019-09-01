object C152 {
  object P1 {
    val mod = 1000000007
    def f(i:Int, acc:BigInt = BigInt(1)):BigInt =
      if(i == 0) acc
      else f(i-1, (acc * i) % mod)

    def isPrime(n:Int):Boolean = n match {
      case 1 => false
      case 2 => true
      case _ =>
        for{i <- 2 to n-1 if n % i == 0} return false
        true
    }
    def numPrimeArrangements(n: Int): Int = {
      val m = (1 to n) count isPrime
      ((f(m, BigInt(1)) * f(n - m, BigInt(1))) % mod).toInt
    }
  }

  object P2 {
    def dietPlanPerformance(A: Array[Int], k: Int, lower: Int, upper: Int): Int = {
      def f(i:Int, acc :Int):Int = {
        println(i, acc)
        if(i+k-1 >= A.length) acc else {
          A.slice(i, i + k ).sum match {
            case x if x > upper => println("x, i",x, i); f(i+1, acc + 1)
            case x if x < lower => f(i+1, acc - 1)
            case _ => f(i+1, acc)
          }
        }
      }
      f(0,0)
    }
  }

  object P3 {
    def canMakePaliQueries(s: String, queries: Array[Array[Int]]): Array[Boolean] = {
      val A = s.toArray
      val fre = Array.fill(s.length + 1)(Array.fill(26)(0))
      for {
        i <- 1 to s.length
        j <- 0 until 26
      }{
        if(A(i-1) - 'a' == j) fre(i)(j) = 1+fre(i-1)(j)
        else fre(i)(j) = fre(i-1)(j)
      }
      fre.map(_.toList) foreach println
      def g(l:Int, r:Int, k:Int):Boolean = {
        println(fre(r+1) zip fre(l) map {case ( x, y) => y - x}  toList)
        (fre(r+1) zip fre(l)).count {case (x, y) => (y - x) % 2 == 1} / 2 <= k
      }
      queries map {case Array(l, r, k) => g(l, r, k)}
    }
  }

  object P4{
    def findNumOfValidWords(words: Array[String], puzzles: Array[String]): List[Int] = {
      val A = words.map(_.toSet).filter(set => set.size <= 7 )
      puzzles.map{
        str => A.count{ arr =>
          val set = str.toSet
          arr.contains(str.head) &&
            arr.forall(x => set.contains(x))
        }
      }.toList
    }
  }
  def main(args: Array[String]): Unit = {
    val str1 = Array("aaaa","asas","able","ability","actt","actor","access")
    val str2 = Array("aboveyz","abrodyz","abslute","absoryz","actresz","gaswxyz")
    println(P4.findNumOfValidWords(str1, str2))
  }
}
