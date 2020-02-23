object C169 {
  object P1 {
    def f(n:Int):Array[Int] = (1 to n).toArray.flatMap{x => Array(x, -x)}
    def sumZero(n: Int): Array[Int] = n % 2 match {
      case 0 => f(n / 2)
      case 1 => f(n / 2) :+ 0
    }
  }

  object P2 {
    type L = List[TreeNode]
    def f(l:L,acc:List[Int]):List[Int] = l match {
      case Nil => acc
      case _ => f(l.flatMap{ x => List(x.left, x.right).filter(_!= null)},
        l.map{x => x.value} ++ acc)
    }
    def g(root:TreeNode):List[Int] = if(root == null) Nil else f(List(root), Nil)
    def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] = {
      (g(root1) ++ g(root2)).sorted
    }
  }

  object P3 {
    def canReach(arr: Array[Int], start: Int): Boolean = {
      def f(l:List[Int], seen:Set[Int]):Boolean = l match {
        case Nil => arr.indices.filter{i => arr(i) == 0} exists seen.contains
        case _=>
          f(l.flatMap{ x => List(x - arr(x), x+arr(x))
            .filter{arr.indices.contains}
            .filterNot{(seen ++ l).contains}
          }, seen ++ l)
      }
      f(List(start), Set.empty[Int])
    }
  }
}
