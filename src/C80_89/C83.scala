object C83 {

  object P1 {
    def largeGroupPositions(s: String): List[List[Int]] = {
      var pre = s(0)
      var pidx = 0
      var ans = List[List[Int]]()
      for(i <- 1 until s.length if s(i)!= pre){
        if(i - pidx >= 3) ans ::= List(pidx, i-1)
        pidx = i
        pre = s(i)
      }
      if(s.length - pidx >= 3)  ans ::= List(pidx, s.length - 1)
      ans.reverse
    }
  }
  object P2 {
    def maskPII(S: String): String = {
      if(S.contains('@')) fname(S) else fnum(S)
    }

    //  "name1@name2.name3"
    def fname(s:String):String = {
      val ch = s.split('@')
      (ch(0).head + "*****" + ch(0).last +"@" + ch(1)).toLowerCase
    }
    def fnum(s:String):String = {
      fnumhelp(s.filter(x => ('0' to '9').toList.contains(x)))
    }
    // 012345
    def last4(s:String):String = {
      s.slice(s.length - 4, s.length)
    }
    def fnumhelp(s:String):String = {
      if(s.length == 10){
        "***-***-" + last4(s)
      }else{
        val h = "*" * (s.length - 10)
        "+"+h +"-***-***-" + last4(s)
      }
    }
  }
  object P3{
    def consecutiveNumbersSum(N: Int): Int =
      1 to (math.sqrt(2 * N + 0.25)  - 0.5).toInt count {i =>
        (2 * N) % i == 0 && (2 * N) / i >= i +1  && ((2 * N) / i  + i -1) % 2 == 0}
  }

  object P4{
    def uniqueLetterString(str:String):Int = {
      val alpha = Array.fill(26)(List(-1,str.length))
      str.zipWithIndex foreach {case (ch,i) => alpha(ch - 'A') ::= i}
      for(i <- 0 until 26) alpha(i) = alpha(i).sorted
      (for{
        i <- 0 until 26
        j <- 1 until alpha(i).length -1
      } yield (alpha(i)(j+1) -alpha(i)(j)) * (alpha(i)(j) - alpha(i)(j-1)))
        .foldLeft(0){(ret, x) => SafeCal.add(ret, x)}
    }
  }
}
