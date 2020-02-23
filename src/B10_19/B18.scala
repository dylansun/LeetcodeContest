object B18 {
  object P1 {
    def arrayRankTransform(arr: Array[Int]): Array[Int] = {
      arr map  arr.distinct.sorted.zipWithIndex.map{case (x,i) => (x,i+1)}.toMap
    }
  }

  object P2 {
    def breakPalindrome(s: String): String = {
      if(s.length <= 1) "" else {
        for{i <- 0 until s.length / 2 if s(i) != 'a'}
          return s.slice(0,i) + 'a' + s.slice(i+1, s.length)
        s.dropRight(1) + 'b'
      }
    }
  }

  object P3 {
    def diagonalSort(mat: Array[Array[Int]]): Array[Array[Int]] = {
      val n = mat.length
      val m = mat(0).length
      (0 until m).map{x => (x, 0)} ++ (1 until m).map{x => (0, x)} foreach{
        case (x,y) =>
          val p = for{d <- 0 to ((m max n)-1) if x+d < n && y + d < m} yield (x+d, y+d)
          val q = p.map{case (a,b) => mat(a)(b)}.sorted
          (p zip q) foreach {case ((a,b),value) => mat(a)(b) = value}
      }
      mat
    }
  }

  // TODO: solve
  object Solution {
    // a b .. c d
    // a c .. b d
    // (a-b) + (c-d)
    // (a-c) + (b-d)
    def maxValueAfterReverse(nums: Array[Int]): Int = {
???
    }
  }
}
