import scala.collection.mutable.{HashMap => HM}
import scala.collection.mutable.{ArrayBuffer => MAB}
object C194 {
  object P1{
    def xorOperation(n: Int, start: Int): Int = {
      (for{i <- 0 until n} yield 2*i + start).foldLeft(0){(ret, x) => ret ^ x}
    }
  }
  object P2 {
    def getFolderNames(names: Array[String]): Array[String] = {
      val table = HM[String, Int]()
      var ret = List.empty[String]
      names foreach {name =>
        if(!table.contains(name)){
          ret ::= name
          table.put(name, 1)
        } else {
          while (table.contains(new_name(name, table(name)))) table(name)+= 1
          ret ::= new_name(name, table(name))
          table(name)+= 1
          table.put(ret.head, 1)
        }
      }
      ret.reverse.toArray
    }

    def new_name(name:String, k:Int):String = name + "("+k+")"
  }
  object P3 {
    case class E(i:Int,j:Int, x:Int)
    val cannot = Array.empty[Int]
    def avoidFlood(rains: Array[Int]): Array[Int] = {
      val last = HM[Int, Int]()
      var l=List.empty[E]
      val pos = MAB[Int]()
      for{i <- rains.indices} {
        if(rains(i) == 0) pos.append(i) else {
          if(last.contains(rains(i))) l::= E(last(rains(i)), i, rains(i))
          last.put(rains(i), i)
        }
      }
      l = l.sortBy{case E(i,j,x) => j}
      val ret = Array.fill(rains.length)(-1)
      l foreach {case E(i,j,x) =>
        if(pos.isEmpty) return cannot
        val k = Bisect.bisect_right(pos, i)
          if(!pos.indices.contains(k) || pos(k) > j) return cannot
          ret(pos(k)) = x
          pos.remove(k)
      }
      pos foreach {i => ret(i) = 1}
      ret
    }
  }
  object P4 {
    type T = Array[Array[Int]]
    val inf = 2000000
    case class E(u:Int,v:Int, w:Int, i:Int)
    def findCriticalAndPseudoCriticalEdges(n: Int, edges: T): List[List[Int]] = {
      var l = List.empty[E]
      var cl = List.empty[Int]
      var fcl = List.empty[Int]
      edges.zipWithIndex foreach {case (Array(u,v,w), i) => l::=E(u,v,w,i)}
      l = l.sortBy{case E(u,v,w,i) => w}
      val min_mst = mst(n, l)
      for{
        i <- l.indices
        new_edges = l.slice(0,i) ++ l.slice(i+1,l.length)
        e = l(i)
      } {
        if(mst(n, new_edges) > min_mst) cl::=e.i else {
          if (mst_with_edge(n, new_edges, e) == min_mst) fcl ::= e.i
        }
      }
      List(cl, fcl)
    }

    def mst_with_edge(n:Int, edges:List[E], e:E):Int = {
      val dsu = new DSU(n)
      dsu.union(e.u, e.v)
      var ret = e.w
      var V = 1
      edges foreach {case E(u,v,w,i) =>
        if(dsu.find(u) != dsu.find(v)){dsu.union(u,v); ret += w; V += 1}
      }
      if(V == n-1)ret else inf
    }
    def mst(n:Int, edges:List[E]):Int = {
      val dsu = new DSU(n)
      var ret = 0
      var V = 0
      edges foreach {case E(u,v,w,i) =>
        if(dsu.find(u) != dsu.find(v)){dsu.union(u,v); ret += w; V += 1}
      }
      if(V == n-1)ret else inf
    }
  }
  def main(args: Array[String]): Unit = {
    /**
      * 输入：n = 5, edges = [[0,1,1],[1,2,1],[2,3,2],[0,3,2],[0,4,3],[3,4,3],[1,4,6]]
        输出：[[0,1],[2,3,4,5]]
      */
    val n = 5
    val edges = Array(
      Array(0,1,1),
      Array(1,2,1),
      Array(2,3,2),
      Array(0,3,2),
      Array(0,4,3),
      Array(3,4,3),
      Array(1,4,6)
    )
    println(P4.findCriticalAndPseudoCriticalEdges(n, edges))
  }
}
