import scala.collection.mutable

object C182 {
  object P1 {
    def findLucky(arr: Array[Int]): Int = {
      val fre = Array.fill(501)(0)
      for{x <- arr} fre(x) += 1
      for{x <- 500 to 1 by -1 if x == fre(x)} return x
      -1
    }
  }
  object P2 {
    def numTeams(rating: Array[Int]): Int = {
      (for{
        i <- rating.indices
        j <- i+1 until rating.length
        k <- j+1 until rating.length
        if (rating(i) > rating(j) && rating(j) > rating(k)) ||
          (rating(i) < rating(j) && rating(j) < rating(k))
      } yield 1).sum
    }
  }
  import scala.collection.mutable.{Set => mset, HashMap =>HM}
  class UndergroundSystem() {
    val cin = new HM[Int, (String, Int)]()
    val c = new HM[(String, String), List[Int]]()
    def checkIn(id: Int, stationName: String, t: Int) {
      cin.put(id, (stationName, t))
    }

    def checkOut(id: Int, stationName: String, t: Int) {
      cin(id) match {case (startStation, startTime) =>
        c.put((startStation, stationName), (t - startTime)::c.getOrElse((startStation, stationName), Nil))
      }
    }

    def getAverageTime(startStation: String, endStation: String): Double = {
      val l = c.getOrElse((startStation, endStation), Nil)
      l.sum.toDouble / l.length
    }

  }
  object Solution {
    def findGoodStrings(n: Int, s1: String, s2: String, evil: String): Int = {
      val ret = solve(n, s2, evil)-solve(n,s1,evil) + (if(s1.contains(evil)) 0 else 1)
      if(ret < 0) ret + 1e9.toInt + 7 else ret
    }
    def solve(n: Int, s1: String, evil: String): Int = {
      val dp = Array.fill(n+1, evil.length, 2)(0)
      val state_trans = Array.fill(evil.length, 26)(-1)
      def f(str1:String, str2:String):Int = {
        if(str1.length == 0) 0
        else if(str2 startsWith str1) str1.length
        else f(str1.tail, str2)
      }
      // brute force solve state trans
      // i: the number of maximum matched character
      for{
        i <- 0 until evil.length
        j <- 0 until 26
        ch = ('a'+j).toChar
      } state_trans(i)(j) = f(evil.slice(0,i)+ch, evil)

      // initial
      dp(0)(0)(1)=1
      for{
        i <- 0 until n
        state <- 0:: (1 until evil.length).toList
        bound <- 0 to 1
        j <- (if(bound == 1) 0 to (s1(i)-'a') else 0 until 26)
        nxt_bound = (if(bound == 1 && j == (s1(i)-'a')) 1 else 0)
      }{
        if(state_trans(state)(j) < evil.length)
        dp(i+1)(state_trans(state)(j))(nxt_bound)=
            SafeCal.add(dp(i+1)(state_trans(state)(j))(nxt_bound),dp(i)(state)(bound))
      }
      SafeCal.sum(dp(n).flatten.toSeq)
    }
  }

}
