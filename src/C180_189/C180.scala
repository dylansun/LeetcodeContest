object C180 {

  def luckyNumbers (matrix: Array[Array[Int]]): List[Int] = {
    def p(i:Int, j:Int):Boolean =
      matrix(i).max == matrix(i)(j) &&
        matrix.indices.forall{k => matrix(k)(j) >= matrix(i)(j)}
    (for{
      i <- matrix.indices
      j <- matrix(i).indices
      if p(i,j)
    } yield matrix(i)(j)). toList
  }
  class CustomStack(_maxSize: Int) {
    type L = List[Int]
    var l = List.empty[Int]
    val maxSize = _maxSize
    def push(x: Int) {
      if(l.length < maxSize) l ::= x
    }

    def pop(): Int = l match {
      case Nil => -1
      case h::t =>
        l = t
        h
    }


    def increment(k: Int, x: Int) {
      def f(l:L, acc:L):L =
        if(l.length <= k) acc.reverse ++ (l map (_+x))
        else f(l.tail, l.head::acc)
      l = f(l, Nil)
    }

  }
  def balanceBST(root: TreeNode): TreeNode = {
    ???
  }
}
