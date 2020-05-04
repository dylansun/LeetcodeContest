case class Point(x:Int, y:Int){
  def +(that:Point):Point = Point(x + that.x, y + that.y)
  def -(that:Point):Point = Point(x - that.x, y - that.y)
}
