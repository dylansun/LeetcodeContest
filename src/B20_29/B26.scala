import scala.collection.mutable.{HashMap => HM, ArrayBuffer => AB}
object B26 {
  object P1 {
    def maxPower(s: String): Int = solve(s.tail.toList, s.head, 1, 1)
    def solve(l:List[Char], pre:Char, ret:Int, cur:Int):Int = l match {
      case Nil => ret
      case h::t =>
        if (h == pre) solve(t, pre, ret max (cur + 1), cur + 1)
        else solve(t, h, ret max cur, 1)
    }
  }

  object P2 {
    def simplifiedFractions(n: Int): List[String] = {
      (for{
        q <- 2 to n
        p <- 1 until q
        if EMath.gcd(p,q) == 1
      } yield p.toString + "/" + q.toString).toList
    }
  }

  object P3 {
    case class E(x:TreeNode, pre:Int)
    def goodNodes(root: TreeNode): Int = {
      def f(e:E):List[E] = e match {
        case E(null, _) => Nil
        case E(root, p) =>
          List(root.left, root.right)
            .filter(_!=null)
            .map { x => E(x, p max root.value)}
      }
      def solve(l:List[E], acc:Int = 0):Int = l match {
        case Nil => acc
        case _ => solve(l flatMap f, acc + l.count{case E(x, p) => x.value >= p})
      }
      if(root == null) 0 else solve(List(E(root, -10001)))
    }
  }
  object P4 {
    def largestNumber(cost: Array[Int], target: Int): String = {
      val mem = HM[Int, List[Int]]()
      mem.put(0, Nil)
      val scost = HM[Int, Int]()
      for(i <- cost.indices) scost.put(cost(i), i+1)
      def f(l1:List[Int], l2:List[Int]):List[Int] = {
        l1.sorted.reverse zip l2.sorted.reverse dropWhile{case (a, b) => a == b} match {
          case Nil => l1
          case (a,b)::t => if(a > b) l1 else l2
        }
      }
      for{
        x <- 1 to target
        y <- scost.keySet
        if x - y >= 0
        if mem.contains(x-y)
      }{
        if(!mem.contains(x)) mem.put(x, scost(y)::mem.getOrElse(x-y, Nil))
        else{
          val nl = scost(y)::mem.getOrElse(x-y, Nil)
          if(nl.length > mem(x).length) mem(x) = nl
          else if(nl.length == mem(x).length){
            mem(x) = f(nl, mem(x))
          }
        }
      }
      if(!mem.contains(target)) "0"
      else mem(target).sorted.reverse.mkString
    }
  }

  def main(args: Array[String]): Unit = {
    val a = List.fill(9)(1)
    val b = a.take(1) ++ ((a(1)+1)::a.drop(2))
    println(b.zipWithIndex.map{case (x,i) => (i+1).toString * x}.reduce(_+_))
  }
}
