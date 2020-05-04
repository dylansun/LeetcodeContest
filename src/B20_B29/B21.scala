object B21 {
  object P1 {
    type LC = List[Char]
    def sortString(s: String): String = {
      def f(l:LC, stack:LC,init:Boolean, acc:LC):LC = (l,stack) match {
        case (Nil, Nil) => acc
        case (Nil, _) => f(stack, Nil,true, acc)
        case (h::t, _) =>
          if(acc.nonEmpty && acc.head == h && !init) f(t,h::stack,false,acc)
          else f(t,stack,false, h::acc)
      }

      f(s.sorted.toList,Nil,true, Nil).reverse.mkString
    }
  }

  object P2 {
    def findTheLongestSubstring(s: String): Int = {
      val M = Map('a'->0,'e'->1,'i'->2,'o'->3,'u'->4)
      val first = Array.fill(32)(-1)
      val last = Array.fill(32)(-1)

      def next(state:Int, ch:Char):Int =
        if(M.contains(ch)) state ^ (1<<M(ch)) else state

      def f(l:List[Char],state:Int, i:Int, acc:Int):Int = l match {
        case Nil => acc
        case h::t =>next(state,h) match {
          case 0 => f(t,0,i+1,i+1)
          case s => first(s) match {
            case -1 =>
              first(s) = i
              f(t, s, i+1, acc)
            case j => f(t, s, i+1, acc max (i-j))
          }
        }
      }
      f(s.toList,0,0,0)
    }
  }

  object P3 {
    type T = TreeNode
    case class E(node:T, dir:Int)// 0 left 1 right
    def longestZigZag(root: TreeNode): Int = {
      val mem = scala.collection.mutable.HashMap[E, Int]()
      def f(x:E):Int = {
        if(mem.contains(x)) return mem(x)
        val ans = x match {
          case E(null, _) => 0
          case E(r, 0) => 1+f(E(r.left, 1))
          case E(r, 1) => 1+f(E(r.right, 0))
          case _ => print(x,"Error");0
        }
        mem.put(x, ans)
        ans
      }
      def solve(root:T):Int = if(root == null) 0 else {
        val l = f(E(root,0))
        val r = f(E(root,1))
        l max r max solve(root.left) max solve(root.right)
      }

      solve(root) -1
    }
  }
  /**
    * Definition for a binary tree node.
    * class TreeNode(var _value: Int) {
    *   var value: Int = _value
    *   var left: TreeNode = null
    *   var right: TreeNode = null
    * }
    */
  object P4 {
    type T = TreeNode
    type P = (Int,Int,Int)
    val MIN = -50000
    val mem = scala.collection.mutable.HashMap[T,P]()
    val mem2 = scala.collection.mutable.HashMap[T,Boolean]()

    def maxSumBST(root: TreeNode): Int = root match {
      case null => 0
      case _ =>
        (if(isBST(root)) lrsBST(root)._3 else 0) max
          maxSumBST(root.left) max maxSumBST(root.right)
    }

    def isBST(root:T):Boolean = if(mem2.contains(root)) mem2(root) else {
      val ret = root match {
        case null => true
        case _ =>
          (if(root.left != null) root.value > lrsBST(root.left)._2 else true) &&
            (if(root.right != null) root.value < lrsBST(root.right)._1 else true) &&
            isBST(root.left) && isBST(root.right)
      }
      mem2.put(root, ret)
      mem2(root)
    }

    def lrsBST(root:T):P = if(mem.contains(root)) mem(root) else {
      val ret = root match {
        case null => (MIN,MIN, 0)
        case _ => (lrsBST(root.left),lrsBST(root.right)) match {
          case ((l1,r1,s1), (l2,r2,s2)) =>
            val l = List(l1,l2,r1,r2,root.value) filter {_!=MIN}
            (l.min,l.max,s1+s2+root.value)
        }
      }
      mem.put(root, ret)
      mem(root)
    }
  }
}
