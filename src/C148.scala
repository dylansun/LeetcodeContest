/**
  * Created by lilisun on 8/4/19.
  */
object C148 {
  object P1 {
    def movesToMakeZigzag(A: Array[Int]): Int = {
      def f(i:Int):Int = {
        A.indices.toList
          .filter(_%2 == i)
          .map{x =>
            0 max (List(x-1, x+1) filter A.indices.contains map A map (y => A(x) - y + 1) max)}
          .sum
      }
      f(0) min f(1)
    }
  }
  object P2 {
    def btreeGameWinningMove(root: TreeNode, n: Int, x: Int): Boolean ={
      List(find(root,x)) flatMap { x:TreeNode =>
          List(n-count(x), count(x.left), count(x.right))
      } exists (_>n/2)
    }
    def count(root:TreeNode):Int = root match {
      case null => 0
      case _ => count(root.left) + count(root.right) + 1
    }

    def find(root:TreeNode, x:Int):TreeNode = {
      if(root == null)  null
      else if(root.value == x)  root
      else find(root.left, x) match {
        case null => find(root.right, x)
        case node => node
      }
    }
  }
  object P3 {

    class SnapshotArray(_length: Int) {
      var snap_id = 0
      val table = scala.collection.mutable.HashMap[(Int, Int), Int]()

      // (Snap_id, index) => value
      def set(index: Int, x: Int) {
        table.put((snap_id, index), x)
      }

      def snap(): Int = {
        snap_id += 1
        snap_id - 1
      }

      def get(index: Int, id: Int): Int = {
        if (id == -1) 0
        else {
          if (table.contains((id, index))) table((id, index))
          else get(index, id - 1)
        }
      }
    }

  }
  object P4 {
    def longestDecomposition(A: String): Int = {
      def f(i:Int, j:Int, acc:Int = 0):Int = {
        if(i > j) acc else {
          g(i,j) match {
            case n => if(j-n+1 == i) f(i+n, j-n, acc+1) else f(i+n, j-n, acc+2)
          }

        }
      }
      def g(i:Int, j:Int, d:Int = 1 ):Int = {
        if(A.substring(i,i+d) == A.substring(j-d+1,j+1)) d
        else g(i,j, d+1)
      }
      f(0, A.length-1, 0)
    }
  }

}
