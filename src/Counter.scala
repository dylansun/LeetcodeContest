import  scala.collection.mutable
class Counter[T]{
  val table =  mutable.HashMap[T,Int]()
  def add(seq: Seq[T]):Unit =
    seq foreach {t => table.put(t, table.getOrElse(t, 0)+1)}

  def items():mutable.HashMap[T, Int] = table
  def get(x:T):Int =
    if(table.contains(x)) table(x) else {table.put(x,0); get(x)}

}

