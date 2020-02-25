object C175 {
  object P1 {
    def checkIfExist(arr: Array[Int]): Boolean = {
      val n = arr.length
      for{
        i <- 0 until n
        j <- i+1 until n
        if arr(i) == 2*arr(j) || arr(i) * 2 == arr(j)
      } return true
      false
    }
  }
  object P2 {
    def f(s:String):Array[Int] = {
      val ret = Array.fill(26)(0)
      s foreach {ch => ret(ch - 'a') += 1}
      ret
    }
    def minSteps(s: String, t: String): Int = {
      (f(s) zip f(t) map {case (x, y) => 0 max (x - y)}). sum
    }
  }

  class TweetCounts() {
    val table = scala.collection.mutable.HashMap[String,List[Int]]()
    def recordTweet(s: String, t: Int) {
      table.put(s, t::table.getOrElse(s, Nil))
    }

    def f(l:List[Int], start:Int,end:Int, delta:Int):List[Int] = {
      val ret = Array.fill((end - start) / delta +1)(0)
      l foreach {x =>
        if(x >= start & x <= end)
          ret((x - start) / delta)+=1
      }
      ret.toList
    }

    def getDelta(freq:String):Int = freq match {
      case "minute" => 60
      case "hour" => 60 * 60
      case "day" => 60*60*24
    }
    def getTweetCountsPerFrequency(freq: String, name: String, start: Int, end: Int): List[Int] = {
      f(table(name), start, end, getDelta(freq))
    }
  }

  
}
