object C172 {

  object P1 {
    def f(l:List[Int], acc:Int = 0):Int = l match {
      case Nil => acc
      case 9::t => f(t, acc * 10 + 9)
      case 6::t => g(t, acc*10 + 9)
      case _ => 0
    }
    def g(l:List[Int], acc:Int):Int = l match {
      case Nil => acc
      case h::t => g(t, acc*10 + h)
    }
    def maximum69Number (x: Int): Int = {
      f(x.toString.toList map {ch => ch - '0'})
    }
  }

  object P2 {
    def f(A:Array[Char]):String = A.reverse.dropWhile{_==' '}.reverse.mkString
    def printVertically(s: String): List[String] = {
      val words = s.split(' ')
      val n = words.map{x => x.length}.max
      val A = Array.fill(n, words.length)(' ')
      for{
        i <- words.indices
        j <- words(i).indices
      } A(j)(i) = words(i)(j)
      (A map f).toList
    }
  }

  object P3 {
    type T = TreeNode
    val parent = scala.collection.mutable.HashMap[T,T]()
    def isLeaf(root:T):Boolean = root!= null && root.left == null && root.right == null
    def removeLeafNodes(root: T, target: Int): T = {
      def f(l:List[T], acc:List[T] = Nil):List[T] = l match {
        case Nil => acc
        case _ =>
          l foreach {r => if(r != null){
            if(r.left != null) parent.put(r.left, r)
            if(r.right != null) parent.put(r.right, r)
          }}
          f(l flatMap {r =>
            if(r == null) Nil
            else List(r.left,r.right).filter(_!=null)
          }, acc ++ l.filter{x => x != null && x.left == null && x.right == null})
      }
      // remove leaf node
      def g(l:List[T]):T = if(l.isEmpty) root else  {
        l.filter{_.value == target} foreach { r => if(parent(r) != null){
          if(parent(r).left == r) parent(r).left = null
          if(parent(r).right == r) parent(r).right = null
        } else return null}
        g(l.filter{_.value == target} map parent filter isLeaf)
      }
      parent.put(root, null)
      g(f(List(root)))
    }
  }

  object P4 {
    def g(n: Int, ranges: Array[Int]):List[Array[Int]] = {
      ranges.zipWithIndex
        .map {case (r, i) => Array(0 max (i-r),(i+r) min n)}
        .sortBy{case Array(a,b) => (a, -b)}.toList
    }
    def minTaps(n: Int, ranges: Array[Int]): Int = {
      def f(l:List[Array[Int]], cur:Int, acc:Int = 0):Int = if(cur == n) acc else {
        l.takeWhile{case Array(a, b) => a <= cur}.map{case Array(a,b) => b} match {
          case Nil =>  -1
          case h::t => f(l.dropWhile{case Array(a,b) => a <= cur}, (h::t).max, acc+1)
        }
      }
      f(g(n,ranges), 0,0)
    }
  }
}
