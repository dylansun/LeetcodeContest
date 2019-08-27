package B1_9

/**
  * Created by lilisun on 7/27/19.
  */
object B5 {
  object p3TLE {
    def minimumCost(N: Int, A: Array[Array[Int]]): Int = {
      val table = scala.collection.mutable.HashMap[(Int, Int), Int]() //(x, y) -> cost
      A foreach {case Array(x,y, cost) => table.put((x,y), cost); table.put((y,x), cost)}
      val edges = scala.collection.mutable.HashMap[Int, List[Int]]()
      A foreach {case Array(x,y, cost) =>
        edges.put(x, y::edges.getOrElse(x, Nil))
        edges.put(y, x::edges.getOrElse(y, Nil))
      }

      var B = A.sortBy{case Array(x,y,cost) => cost}
      var l1 = List.empty[Int]
      var need = 0
      B.head match {case Array(x, y, cost) =>
        l1 = x::y::Nil
        need += cost
      }

      while(l1.length != N){
        var next = -1
        var next_cost = Int.MaxValue
        for{
          p <- l1
          q <- edges(p)
          if ! l1.contains(q)
        }{
          if(table((p,q)) < next_cost){
            next = q
            next_cost = table((p,q))
          }
        }
        if(next == -1) return -1
        l1 ::= next
        need += next_cost
      }
      need
    }
  }
  object p3 {
    class DSU(N:Int){
      val parent = (0 until N).toArray
      def find(x:Int):Int = {
        val tmp = (if(x != parent(x)) find(parent(x)) else x)
        parent(x) = tmp
        tmp
      }
      def union(x:Int, y:Int):Unit = {parent(find(x)) = parent(find(y))}
    }
    def minimumCost(N: Int, A: Array[Array[Int]]): Int = {
      val dsu = new DSU(N)
      var m = 0
      A.sortBy{case Array(x,y,cost) => cost} foreach {
        case Array(x,y,cost) =>
          if(dsu.find(x-1) != dsu.find(y-1)){
            m += cost
            dsu.union(x-1,y-1)
          }
      }
      if((0 until N).toList.map(x => dsu.find(x)).distinct.length == 1) m
      else -1
    }
  }
  def main(args: Array[String]): Unit = {
    val A = Array(Array(1,2,5), Array(1,3,6), Array(2,3,1))
    println(p3.minimumCost(3, A))
  }
}
