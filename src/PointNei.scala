object PointNei{
  val dir4 = List(Point(0,1), Point(0,-1), Point(1,0), Point(-1, 0))
  val dir8 = List(Point(-1,-1), Point(-1,1), Point(1,-1),Point(1,1)) ++ dir4

  def nei4(n:Int, m:Int)(p:Point):List[Point] =
    dir4 map(_+p) filter inBound(n,m)

  def nei8(n:Int, m:Int)(p:Point):List[Point] =
    dir8 map(_+p) filter inBound(n,m)

  def inBound(n:Int,m:Int)(p:Point):Boolean =
    p.x < n && p.y < m && p.x>=0 && p.y >=0
}
