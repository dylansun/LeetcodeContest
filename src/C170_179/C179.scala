object C179 {
  object P1 {
    def generateTheString(n: Int): String = n % 2 match {
      case 0 => "a"*(n-1) + "b"
      case 1 => "a"*n
    }
  }
  object P2 {
    def numTimesAllBlue(light: Array[Int]): Int = {
      def fp(y:Int, n:Int):Int =
        if(n==light.length || y == n+1) 1
        else 0
      def f(seq:Seq[Int],n:Int, y:Int, acc:Int):Int =
        if(seq.isEmpty) acc
        else f(seq.tail, n-1, y min seq.head , fp(y, n)+acc)
      f(light.reverse, light.length, light.length + 1, 0)
    }
  }
import language.postfixOps
  object P3 {
    case class E(id:Int, cost:Int){
      def +(that:E):E = E(that.id, cost + that.cost)
    }
    def numOfMinutes_topdown(n: Int, headID: Int, manager: Array[Int], informTime: Array[Int]): Int = {
      val graph = new Graph[Int]()
      for {i <- manager.indices}{graph.add(manager(i), i)}
      def f(l:List[E], acc:Int):Int = l match {
        case Nil => acc
        case _ =>
          l partition {case E(x,_) => graph.get(x).nonEmpty} match {case (l1,l2) =>
            f(l1 flatMap {case E(id,cost)=> graph.get(id) map(y => E(y, informTime(y))) map(E(id,cost)+_)}, (acc::(l2 map (_.cost))).max)
          }
      }
      f(List(E(headID, informTime(headID))), 0)
    }

    def numOfMinutes(n: Int, headID: Int, manager: Array[Int], informTime: Array[Int]): Int = {
      def f(x:Int, acc:Int = 0):Int = if(x == headID) acc + informTime(x) else f(manager(x), informTime(x)+acc)
      (for {x <- 0 until n if informTime(x)==0} yield f(x)) max
    }
  }

  object Solution {
    def frogPosition(n: Int, edges: Array[Array[Int]], t: Int, target: Int): Double = {
      val graph = new Graph[Int]()
      edges foreach {case Array(x,y)=> graph.biadd(x,y)}
      var ret = 0.0
      def dfs(pos:Int, prob:Double, path:Set[Int],step:Int):Unit = {
        val nxt = graph.get(pos) filterNot path.contains
        if(nxt.isEmpty || step == 0){if(pos ==target) ret += prob}
        else nxt foreach {x => dfs(x, prob / nxt.length, path + x, step -1)}
      }
      dfs(1, 1.0, Set(1), t)
      ret
    }
  }

  def main(args: Array[String]): Unit = {
    //println(P1.generateTheString(4))
    //println(P2.numTimesAllBlue(Array(3,2,4,1,5)))
    println(P3.numOfMinutes(7,6,Array(1,2,3,4,5,6,-1), Array(0,6,5,4,3,2,1)))
    println(P3.numOfMinutes(6,2,Array(2,2,-1,2,2,2), Array(0,0,1,0,0,0)))

  }
}
