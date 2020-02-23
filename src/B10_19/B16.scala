object B16 {
  object p1 {
    def replaceElements(arr: Array[Int]): Array[Int] = {
      def f(l:List[Int], l0:List[Int], x:Int):List[Int] = l match {
        case Nil => l0
        case h::t => f(t, x::l0, h max x)
      }
      f(arr.toList.reverse, Nil, -1).toArray
    }
  }

  object p2 {
    def findBestValue(A: Array[Int], target: Int): Int = {
      def check( k:Int):Boolean = {
        A.map {x => x min k}.sum <= target
      }
      def search(l:Int, r:Int, acc:Int = 0):Int = if(l > r) acc else
      {
        val mid = (l + r) / 2
        if(check(mid)) search(mid+1,r, mid)
        else search(l, mid-1, acc)
      }
      def calc( k:Int):Int = {
        Math.abs(A.map {x => x min k}.sum - target)
      }
      def adjust(x:Int):Int = if(calc(x) <= calc(x+1)) x else x+1
      adjust(search(0, A.max))
    }
    object p3 {
      type T = TreeNode
      def deepestLeavesSum(root: TreeNode): Int = {
        def f(l0:List[T], l1:List[T]):Int = l0 match {
          case Nil => l1.map{x => x.value}.sum
          case _ =>
            f(l0.flatMap{x => List(x.left, x.right).filterNot{x => x==null}}, l0)
        }
        f(List(root), Nil)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val A = Array(2,3,5)
    val B = Array(60864,25176,27249,21296,20204)
    println(p2.findBestValue(A,10))
    println(p2.findBestValue(B,56803))
  }
}
