
import scala.collection.mutable
import scala.language.postfixOps
object C145 {

  // P1 code
    def relativeSortArray(A: Array[Int], B: Array[Int]): Array[Int] = {
      val t1 = mutable.HashMap[Int, Int]()
      val t2 = mutable.HashMap[Int, Int]()

      B foreach {x => t1.put(x, 0)}
      A foreach { x =>
        if(t1.contains(x)) t1(x) += 1
        else t2.put(x, 1+ t2.getOrElse(x, 0))
      }

      var l = List.empty[Int]
      B foreach {x => l = if(t1.contains(x)) l ++ List.fill(t1(x))(x) else l}
      (l ++ (A filterNot t1.contains)).toArray
    }

  // p2 code
  def lcaDeepestLeaves(root: TreeNode): TreeNode = {
    if(root == null) null
    else {
      val n1 = depth(root.left)
      val n2 = depth(root.right)
      if(n1 == n2) root
      else if (n1 > n2) lcaDeepestLeaves(root.left)
      else lcaDeepestLeaves(root.right)
    }
  }

  def depth(root:TreeNode):Int = {
    if (root == null) 0
    else 1 + (depth(root.left) max depth(root.right))
  }

  // p3 code
  def longestWPI(hours: Array[Int]): Int = {
    val A = hours.map(x => if(x > 8) 1 else -1)
    val B = Array.fill(A.length)(0)
    B(0) = A(0)
    A.indices.tail foreach {i => B(i) = B(i-1) +A(i)}
    var max_len = 0
    B.indices foreach {i => if(B(i) > 0) max_len = i+1}
    for{
      i <- B.indices
      j <- i+1 until B.length
      if B(j) - B(i) > 0
    } max_len = max_len max (j-i)
    max_len
  }

  // p4 code
  def smallestSufficientTeam(A: Array[String], ll: List[List[String]]): Array[Int] = {
    def f(l:List[String]):Int = {
      var x = 0
      for {
        y <- (A map l.contains).zipWithIndex
        if y._1
      } x += 1 << y._2
      x
    }
    def cond(x:Int)(y:Int):Boolean = (x & y) != 0 && (y & ~(x & y)) == 0
    def g(x:Int):List[Int] = (1 to x).toList.filter {y => cond(x)(y)}
    val table = mutable.HashMap[Int, List[Int]]()
    val track = mutable.HashMap[Int, Int]()
    val path  = mutable.HashMap[Int, (Int, Int)]() // target, (source, choice)
    val l = ll map f
    l.zipWithIndex foreach {case (x, i) => track.put(x,i)}
    l foreach {q => table.put(q, g(q))}

    val dp = Array.fill(1 << A.length)(61)
    dp(0) = 0
    for{ i <- 1 until (1 << A.length)}{
      var c_min = 61
      var from_q = -1
      var from_p = -1
      for{
        q <- l
        k <- table(q)
        p = i & (~k)
      } {
        if (dp(p) + 1 < c_min){
          c_min = dp(p) + 1
          from_p = p
          from_q = q
        }
      }

      if(c_min < dp(i)){
        dp(i) = c_min
        path.put(i, (from_p, track(from_q)))
      }
    }
    def getPath(i:Int, acc:List[Int]):List[Int] = {
      if(i == 0) acc
      else getPath(path(i)._1, path(i)._2 ::acc)
    }
    getPath(dp.indices.last, Nil).toArray
  }
}
