package B1_9

/**
  * Created by lilisun on 8/25/19.
  */
object B7 {
  object P1 {
    def calculateTime(A: String, w: String): Int = {
      val map = scala.collection.mutable.HashMap[Char, Int]()
      A.toArray.zipWithIndex foreach {case (ch, i) => map.put(ch, i)}
      ((w zip w.tail) map {case (a, b) => Math.abs(map(a)-map(b))} sum)+map(w.head)
    }
  }

  object P2 {
    class FileSystem() {
      // Initial
      val mem = scala.collection.mutable.HashMap[String, Boolean]()
      val mem_value = scala.collection.mutable.HashMap[String, Int]()
      mem.put("", true)


      def valid(path:String):Boolean = {
        if(path == "" || path == "/") false
        else path.head == '/'
      }
      def parent(path:String):String = {
        path.split("/").dropRight(1).mkString("/")
      }
      def create(path: String, value: Int): Boolean = {
        if(valid(path) && mem.contains(parent(path))){
          if(mem_value.contains(path)) false else {
            mem.put(path, true)
            mem_value.put(path, value)
            true
          }
        }else false
      }

      def get(path: String): Int = {
        if(mem_value.contains(path)) mem_value(path) else -1
      }

    }
  }
  object P3 {
    def connectSticks(A: Array[Int]): Int = {
      val pq = scala.collection.mutable.PriorityQueue[Int]()(Ordering.by(x => -x))
      A foreach {x => pq.enqueue(x)}
      var cost = 0
      while(pq.size > 1){
        val a = pq.dequeue
        val b = pq.dequeue
        cost += a+b
        pq.enqueue(a+b)
      }
      cost
    }
  }

  object P4 {
    class DSU(N:Int){
      val parent = (0 until N).toArray
      def find(x:Int):Int = {
        val tmp = if(x != parent(x)) find(parent(x)) else x
        parent(x) = tmp
        tmp
      }
      def union(x:Int, y:Int):Unit = {parent(find(x)) = parent(find(y))}
    }
    def minCostToSupplyWater(n: Int, wells: Array[Int], pipes: Array[Array[Int]]): Int = {
      val B = ( wells.zipWithIndex map {case (x,i) => Array(0, i+1, x)}) ++ pipes
      val dsu = new DSU(n+1)
      var cost = 0
      B.sortBy{case Array(x,y,z) => z} foreach {case Array(x, y, c) =>
        if(dsu.find(x) != dsu.find(y)){
          dsu.union(x, y)
          cost += c
        }
      }
      cost
    }
  }
}
