import scala.collection.mutable.{HashMap => HM}
class Graph [T]{
  val graph = HM[T,List[T]]()
  def add(src:T,dst:T):Unit = graph.put(src, dst::graph.getOrElse(src, Nil))
  def biadd(x:T,y:T):Unit = {add(x,y); add(y,x)}
  def items(): HM[T,List[T]] = graph
  def get(x:T):List[T] = if(graph.contains(x)) graph(x) else {graph.put(x, Nil); get(x)}
}
