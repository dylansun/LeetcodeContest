package B10_19

/**
  * Created by lilisun on 11/4/19.
  */
object B12 {
  object P1{
    class Leaderboard() {

      val board = scala.collection.mutable.HashMap[Int, Int]()
      def addScore(id: Int, score: Int) {
        board.put(id, board.getOrElse(id, 0) + score)
      }

      def top(k: Int): Int = {
        board.values.toList.sortBy{x => -x}.slice(0,k).sum
      }

      def reset(id: Int) {
        board.put(id, 0)
      }

    }
  }

  object P2 {
    def transformArray(A: Array[Int]): List[Int] = {
      val B = Array.fill(A.length)(0)
      val n = A.length
      B(0) = A(0)
      B(n-1) = A(n-1)
      def update():Boolean = {
        A.indices.tail.dropRight(1) foreach {i =>
          B(i) = if(A(i) > A(i-1) && A(i) > A(i+1)) A(i)-1
          else if(A(i) < A(i-1) && A(i) < A(i+1)) A(i)+1
          else A(i)

        }
        if(A.toList == B.toList) true else {
          A.indices.tail.dropRight(1) foreach {i => A(i) = B(i)}
          false
        }
      }

      def solve():List[Int] = update() match {
        case false => solve()
        case true => A.toList
      }
      solve()
    }
  }

  object P3 {
    def treeDiameter(edges: Array[Array[Int]]): Int = {
      val table = scala.collection.mutable.HashMap[Int, Int]()
      val neigh = scala.collection.mutable.HashMap[Int, List[Int]]()
      def dfs(l:List[Int], visited: Set[Int], n :Int):Unit = l match {
        case Nil => {}
        case h::t =>
          l foreach {x => table.put(x, n)}
          dfs(l flatMap {x => neigh(x) filterNot visited.contains}, visited ++ l, n+1)
      }
      edges foreach { case Array(a, b) =>
        neigh.put(a, b::neigh.getOrElse(a, Nil))
        neigh.put(b, a::neigh.getOrElse(b, Nil))
      }
      dfs(List(0), Set.empty[Int], 0)
      var m = 0
      var start = 0
      table foreach {case (k, v) => if(v > m) {start = k; m = v}}
      dfs(List(start), Set.empty[Int], 0)
      table.values.max
    }
  }

  object P4 {
    def minimumMoves(A: Array[Int]): Int = {
      val n = A.length
      val dp = Array.fill(n, n)(n)
      (0 until n) foreach {i => dp(i)(i) = 1}
      for{
        i<- 1 until n
        j<- 0 until n
        if j+i < n
      }{
        if(A(j) == A(j+i)) i match {
          case 1 => dp(j)(j+1) = 1
          case _ => dp(j)(j+i) = dp(j+1)(j+i-1)
        } else {
          for{k <- j until j+i}
            dp(j)(j+i) = dp(j)(j+i) min (dp(j)(k) + dp(k+1)(j+i))
        }
      }
      dp(0)(n-1)
    }
  }
}
