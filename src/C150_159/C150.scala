/**
  * Created by lilisun on 8/18/19.
  */
object C150 {
  object P1 {
    def countCharacters(words: Array[String], chars: String): Int = {
      def toVec(s:String):Array[Int] = {
        val A = Array.fill(26)(0)
        s foreach {ch => A(ch - 'a') += 1}
        A
      }

      def f(A:Array[Int])(s:String):Int = {
        if((A zip toVec(s)) forall {case (x,y) => x >= y}) s.length else 0
      }
      words map f(toVec(chars)) sum
    }
  }

  object P2 {
    type T = TreeNode
    def maxLevelSum(root: TreeNode): Int = {
      def getSum(l:List[T]):Int = l.map(_.value) sum
      def next(l:List[T]):List[T] = l.flatMap (x => List(x.left, x.right).filter(_!=null))
      def f(l:List[T],cur:Int, max_sum:Int = Int.MinValue, idx:Int =0 ):Int = l match {
        case Nil => idx
        case _ =>
          if(getSum(l) > max_sum) f(next(l), cur+1, getSum(l), cur)
          else f(next(l), cur+1, max_sum, idx)
      }

      f(List(root), 1)
    }
  }

  object P3 {
    case class P(x:Int, y:Int){
      def +(that:P):P = P(x + that.x, y + that.y)
    }
    val neigh = List(P(0,1), P(0,-1), P(-1,0), P(1,0))
    def maxDistance(grid: Array[Array[Int]]): Int = {
      val n = grid.length
      val m = grid(0).length
      val l = for{
        i <- 0 until n
        j <- 0 until m
        if grid(i)(j) == 1
      } yield P(i,j)

      if(l.isEmpty || l.length == m * n) return -1

      def inBound(p:P):Boolean = {
        grid.indices.contains(p.x) &&
          grid(p.x).indices.contains(p.y)
      }

      def nei(p:P):List[P]= {
        neigh.map(_+p) filter inBound
      }

      def get(p:P):Int = grid(p.x)(p.y)
      def solve(l:Seq[P],v:Set[P],acc:Int):Int = {
        if(v.size == n * m)  acc
        else {
          val nl = for{
            x <- l
            y <- nei(x)
            if !v.contains(y)
          }yield y
          solve(nl.distinct, v ++ nl,acc + 1)
        }
      }
      solve(l, l.toSet, 0)
    }
  }

  // abcad
  // a
  // b d
  // c
  // a
  object P4 {
    def lastSubstring(s: String): String = {
      if(s.distinct.length == 1) return s
      def g(i:Int, k:Int, d:Int):Int = {
        if(i + d >= s.length) k
        else if(s(i+d) == s(k+d)) g(i,k, d+1)
        else if(s(i+d) > s(k+d)) i
        else k
      }
      var ans = s.length -1
      for { i <- (s.length -2 to 0 by -1) }{
        if(s(i) > s(ans)) ans = i
        else if (s(i) < s(ans)) {}
        else {
          if(ans - i == 1) ans = i
          else ans = g(ans, i, 1)
        }
      }

      s.slice(ans, s.length)
    }
  }

}
