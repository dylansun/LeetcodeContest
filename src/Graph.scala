import scala.collection.mutable.{HashMap => HM}
class Graph [T]{
  val graph = HM[T,List[T]]()
  def add(x:T,y:T):Unit = graph.put(x, y::graph.getOrElse(x, Nil))
  def biadd(x:T,y:T):Unit = {add(x,y); add(y,x)}
  def items(): HM[T,List[T]] = graph
  def get(x:T):List[T] = if(graph.contains(x)) graph(x) else {graph.put(x, Nil); get(x)}
}
