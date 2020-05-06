object PointNei{
  val dir4 = List(Point(0,1), Point(0,-1), Point(1,0), Point(-1, 0))
  val dir8 = List(Point(-1,-1), Point(-1,1), Point(1,-1),Point(1,1)) ++ dir4

  def nei4(row:Int, col:Int)(p:Point):List[Point] =
    dir4 map(_+p) filter inBound(row,col)

  def nei8(row:Int, col:Int)(p:Point):List[Point] =
    dir8 map(_+p) filter inBound(row,col)

  def inBound(row:Int,col:Int)(p:Point):Boolean =
    p.x < row && p.y < col && p.x>=0 && p.y >=0
}
