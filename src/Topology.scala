import scala.collection.mutable.{HashMap => HM}
case class Edge[T](src:T, dst:T)
class Topology[T] {
  def isUnique(ret:List[List[T]]):Boolean = ret forall (_.length ==1)
  def build_graph(l:List[Edge[T]], inDegrees:HM[T, Int]):(HM[T, Int], Graph[T]) = {
    val graph = new Graph[T]()
    l foreach {case Edge(x, y) => graph.add(x,y); inDegrees(y) += 1}
    (inDegrees, graph)
  }
  def build_graph_seqs(l:List[List[T]], inDegrees:HM[T, Int]):(HM[T, Int], Graph[T]) = {
    val graph = new Graph[T]()
    def f(l:List[T]):Unit = l match {
      case h1::h2::t => graph.add(h1, h2); f(h2::t)
      case _ => {}
    }
    for{ll <- l} f(ll)
    (inDegrees, graph)
  }
  def topology_sort(inDegrees:HM[T,Int], graph:Graph[T]):(List[List[T]], Boolean) = {
    val n = inDegrees.size
    val npoints = 0
    var ret = List.empty[List[T]]
    var Q = (for{v <- inDegrees.keySet if inDegrees == 0} yield v).toSeq
    while(Q.nonEmpty){
      var nQ = Seq.empty[T]
      npoints += Q.length
      ret ::= Q.toList
      for{v <- Q; x <- graph.get(v)}{
        inDegrees(x) -= 1
        if(inDegrees(x) == 0) nQ = nQ :+ x
      }
      Q = nQ
    }
    (ret.reverse, npoints == n)
  }
  def min_topology_sort_steps(inDegrees:HM[T,Int], graph:Graph[T], limit:Int):Int = {
    val n = inDegrees.size
    var npoints = 0
    var r = 0
    var Q = (for{v <- inDegrees.keySet if inDegrees == 0} yield v).toSeq
    def f(v:T):Int = graph.get(v).count(x => inDegrees(x) == 1)
    while(Q.nonEmpty){
      Q = Q.sortBy{x => -f(x)}
      var nQ = if(Q.length <= limit) Seq.empty[T] else Q.slice(limit,Q.length)
      Q = Q.slice(0, limit)
      r += 1
      npoints += Q.length
      for{v <- Q; x <- graph.get(v)}{
        inDegrees(x) -= 1
        if(inDegrees(x) == 0) nQ = nQ :+ x
      }
      Q = nQ
    }
    if(npoints == n) r else -1
  }
}
