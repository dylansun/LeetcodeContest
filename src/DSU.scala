class DSU(_size:Int) {
  val parent = (0 until _size).toArray
  def find(x:Int):Int = if(x == parent(x)) x else find(parent(x))
  def union(x:Int, y:Int):Unit = {parent(find(x)) = parent(find(y))}
}
