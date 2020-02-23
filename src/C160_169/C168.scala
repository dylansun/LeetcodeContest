object C168 {
  object p1 {
    def findNumbers(nums: Array[Int]): Int = {
      nums.count(x => x.toString.length % 2 == 0)
    }
  }

  object p2 {
    //
    def f(k:Int)(l:List[Int],l0:List[Int],  cnt:Int, x:Int):Boolean = l match {
      case Nil => cnt match {
        case 0 =>
          if(l0.isEmpty) true
          else f(k)(l0.reverse, Nil, k, l0.last)
        case _ => false
      }
      case h::t => cnt match {
        case 0 =>
          if(l0.isEmpty) f(k)(l, Nil, k, h)
          else f(k)(l0.reverse ++ l, Nil, k, l0.last)
        case _ =>
          if (h > x) false
          else if (h == x) f(k)(t, l0, cnt - 1, x + 1)
          else f(k)(t, h :: l0, cnt, x)
      }
    }
    def isPossibleDivide(nums: Array[Int], k: Int): Boolean = nums.sorted.toList match {
      case Nil => true
      case h::t => f(k)(h::t, Nil, k, h)
    }
  }

  object p3 {
    def maxFreq(s: String, maxLetters: Int, minSize: Int, maxSize: Int): Int = {
      val table = scala.collection.mutable.HashMap[String, Int]()
      for{
        i <- 0 until s.length
        j = minSize
        if i+j-1 < s.length
        str = s.slice(i,i+j-1)
        if str.distinct.length <= maxLetters
      } table.put(str, table.getOrElse(str, 0) + 1)
      if(table.isEmpty) 0 else table.values.max
    }
  }
  object p4 {
    def maxCandies(status: Array[Int], candies: Array[Int], keys: Array[Array[Int]], containedBoxes: Array[Array[Int]], initialBoxes: Array[Int]): Int = {
      def  f(l:List[Int], acc:Int):Int =
        if(!l.exists{x => status(x) == 1}) acc else
        {
          val l0 = l.filter{x => status(x) == 1}
          l0 foreach {x => keys(x) foreach {y => status(y) = 1}}
          val nl = l.filterNot{l0.contains} ++ l0.flatMap{x => containedBoxes(x)}
          f(nl, acc + (l0 map candies).sum)
        }


      f(initialBoxes.toList, 0)
    }
  }
}
