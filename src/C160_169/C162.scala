package C160_169

/**
  * Created by lilisun on 11/10/19.
  */
object C162 {
  object P1 {
    def oddCells(n: Int, m: Int, indices: Array[Array[Int]]): Int = {
      val mat = Array.fill(n,m)(0)
      indices foreach {case Array(r,c) =>
        for{i <- 0 until n} mat(i)(c) += 1
        for{j <- 0 until m} mat(r)(j) += 1
      }
      var cnt = 0
      for{
        i <- 0 until n
        j <- 0 until m
      } cnt += (mat(i)(j) % 2)
      cnt
    }
  }

  // TLE last case
  // use c++ to pass 
  object P2 {
    def reconstructMatrix(upper: Int, lower: Int, colsum: Array[Int]): List[List[Int]] = {
      val n = colsum.length
      val A = Array.ofDim[Int](n)
      val B = Array.ofDim[Int](n)
      def show(): Unit = {
        println(s"A:${A.toList}\n B:${B.toList}")
      }

      def solve(i: Int, acc: List[Int], upper: Int, lower: Int): List[List[Int]] =
        if (i < n) colsum(i) match {
          case 2 => A(i) = 1; B(i) = 1; solve(i + 1, acc, upper - 1, lower - 1)
          case 1 => solve(i + 1, i :: acc, upper, lower)
          case 0 => solve(i + 1, acc, upper, lower)
        } else g(acc, upper, lower)

      def g(l: List[Int], upper: Int, lower: Int): List[List[Int]] = l match {
        case Nil => if (upper != 0 || lower != 0) Nil else List(A.toList, B.toList)
        case i :: t =>
          if (upper > 0) {
            A(i) = 1
            g(t, upper - 1, lower)
          } else {
            B(i) = 1
            g(t, upper, lower - 1)
          }
      }
      solve(0,Nil, upper, lower)
    }
  }
  object P3 {
    case class Pos(x:Int, y:Int)
    val dir = List(Pos(1,0), Pos(-1,0), Pos(0,-1), Pos(0,1))
    def closedIsland(grid: Array[Array[Int]]): Int = {
      val visited = scala.collection.mutable.HashMap[Pos, Boolean]()
      val n = grid.length
      val m = grid(0).length
      var cnt = 0
      def dfs(l:List[Pos], outflag:Boolean = false):Unit = l match {
        case Nil => if(outflag) cnt += 1
        case h::t =>
          visited.put(h, true)
          val neigh = g(h)
          // if all point in bound
          if(neigh forall check){
            dfs((neigh filter notOne filterNot visited.contains) ++ t, outflag)
          } else {
            dfs((neigh filter check filter notOne filterNot visited.contains) ++ t, true)
          }
      }
      def g(p:Pos):List[Pos] = {
        dir map {case Pos(dx, dy) => Pos(p.x + dx, p.y + dy)}
      }
      def check(p:Pos):Boolean = {
        p.x >= 0 &&
          p.y >= 0 &&
          p.x < n &&
          p.y < m
      }
      def notOne(p:Pos):Boolean = grid(p.x)(p.y) == 0
      for{
        i <- grid.indices
        j <- grid(i).indices
        p = Pos(i,j)
        if notOne(p)
      } if(! visited.contains(p)) dfs(p::Nil)

      cnt
    }
  }

  object P4 {
    case class Fre(A:Array[Int]){
      def -(that:Fre):Fre = Fre(A zip that.A map {case (x, y) => x - y})
      def >(that:Fre):Boolean = A zip that.A forall {case (x, y) => x >= y}
    }
    def w2f(str:String):Fre = {
      val A = Array.fill(26)(0)
      str foreach {ch => A(ch-'a') += 1}
      Fre(A)
    }
    def maxScoreWords(words: Array[String], letters: Array[Char], score: Array[Int]): Int = {
      val n = words.length
      val weights = words map {word => word map {ch => score(ch - 'a')} sum}
      val wf = words map w2f
      val lf = w2f(letters.mkString)
      var ans = 0
      def backtrack(l:List[(Fre, Int)], pack:Fre , total:Int):Unit = l match {
        case Nil => ans = ans max total
        case (fre, point)::t =>
          ans = ans max total
          if(pack > fre) {
            backtrack(t, pack, total)
            backtrack(t, pack - fre, total + point)
          } else backtrack(t, pack, total)
      }
      backtrack(wf zip weights toList, lf, 0)
      ans
    }
  }

  def test4():Unit = {
    val grid = Array(
      Array(1,1,1,1,1,1,1,0),
      Array(1,0,0,0,0,1,1,0),
      Array(1,0,1,0,1,1,1,0),
      Array(1,0,0,0,0,1,0,1),
      Array(1,1,1,1,1,1,1,0)
    )
    println(P3.closedIsland(grid))
  }
  def main(args: Array[String]): Unit = {
    val A = Array.ofDim[Int](10)
    println(A.indices.toIterator)

  }
}
