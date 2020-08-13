object C171 {
  object P1 {
    def p(x:Int):Boolean = !x.toString.contains('0')
    def getNoZeroIntegers(n: Int): Array[Int] = {
      for{i <- 1 to n/ 2 if p(i) && p(n-i)}
        return Array(i,n-i)
      Array(-1,-1)
    }
  }

  object P2 {
    def f(a:Int, b:Int, c:Int, acc:Int = 0):Int = (a, b, c) match {
      case (0,0,0) => acc
      case (_,_,_) =>
        val d = (c & 1, b&1,a&1) match {
          case (1,0,0) => 1
          case (0,x,y) => x + y
          case (_,_,_) => 0
        }
        f(a>>1,b>>1,c>>1, d + acc)
    }
    def minFlips(a: Int, b: Int, c: Int): Int = f(a,b,c)
  }

  object P3 {
    type T = Array[Array[Int]]
    class DSU(n:Int){
      val A = (0 until n).toArray
      def find(x:Int):Int = if(A(x) == x) x else find(A(x))
      def union(x:Int, y:Int):Unit = A(find(x)) = find(y)
    }
    def makeConnected(n: Int, conn: T): Int = {
      if(n > conn.length + 1) return -1
      val dsu = new DSU(n)
      conn foreach {case Array(x,y) => dsu.union(x,y)}
      ((0 until n) map dsu.find)
        .distinct.length -1
    }
  }
  object P4 {
    case class P(x:Int, y:Int){
      def -(that:P):P = P(x - that.x , y - that.y)
      def distToOrigin():Int = math.abs(x) + math.abs(y)
    }
    def pos(x:Int):P = P(x/6, x % 6)
    def minimumDistance(word: String): Int = {
      val n = word.length
      val dp = Array.fill(n + 1, 26,26)(1000000)
      for{i <- 0 until 26; j <- 0 until 26} dp(0)(i)(j) = 0
      for{
        i <- 1 to n
        x = word(i-1) - 'A'
      } {
        for{j <- 0 until 26} // fix x or fix j
          dp(i)(x)(j) = ((for{k <- 0 until 26} yield dp(i-1)(k)(j) + (pos(k) - pos(x)).distToOrigin) ++
            (for{k <- 0 until 26} yield dp(i-1)(x)(k) + (pos(k) - pos(j)).distToOrigin)).min
        for{j <- 0 until 26}
          dp(i)(j)(x) = ((for{k <- 0 until 26} yield dp(i-1)(k)(j) + (pos(k) - pos(x)).distToOrigin) ++
            (for{k <- 0 until 26} yield dp(i-1)(x)(k) + (pos(k) - pos(j)).distToOrigin)).min
      }
      dp(n).flatten.min
    }
  }
}
