/**
  * Created by lilisun on 9/29/19.
  */
object C156 {
  object Solution {
    case class Pos(x:Int, y:Int){
      def +(that:Pos):Pos = Pos(x+ that.x, y + that.y)
    }
    case class state(r:Pos, c:Pos){
      def +(that:Pos):state = state(r+that, c + that)
      def isVert():Boolean = r.x == c.x
    }
    def minimumMoves(grid: Array[Array[Int]]): Int = {
      val n = grid.length
      val mem = scala.collection.mutable.HashMap[state, Int]() // pos -> move count
      val start = state(Pos(0,0),Pos(0,1))
      val finish =state(Pos(n-1,n-2),Pos(n-1,n-1))
      mem.put(start, 0)
      println(mem.get(start))
      def fetch(pos:Pos):Boolean = if(inBound(pos)) grid(pos.x)(pos.y) == 0 else false
      def inBound(pos:Pos):Boolean = {
        pos.x >= 0 &&
          pos.x < n &&
          pos.y >= 0&&
          pos.y < n
      }
      def moveRight(s:state):Option[state] = {
        if(s.isVert() &&  fetch(s.c + Pos(0,1)))
          Some(s + Pos(0, 1))
        else if(!s.isVert() && fetch(s.c + Pos(0,1)) && fetch(s.r + Pos(0,1)))
          Some(s + Pos(0,1))
        else None
      }
      def moveDown(s:state):Option[state] = {
        if(!s.isVert() && fetch(s.c + Pos(1,0)))
          Some(s + Pos(1,0))
        else if(s.isVert() && fetch(s.c + Pos(1,0)) && fetch(s.r + Pos(1,0)))
          Some(s + Pos(1,0))
        else None
      }

      def rotateClockwise(s:state):Option[state] = {
        if(s.isVert && fetch(s.r + Pos(1,0)) && fetch(s.c + Pos(1,0)))
          Some(state(s.r, s.r + Pos(1,0))) else None
      }

      def rotateCounterClockwise(s:state):Option[state] = {
        if(!s.isVert && fetch(s.r + Pos(0,1)) && fetch(s.c + Pos(0,1)))
          Some(state(s.r, s.r + Pos(0,1))) else None
      }
      def find(x:state):List[state] = {
        List(moveRight(x), moveDown(x), rotateClockwise(x), rotateCounterClockwise(x))
          .filterNot {x => x == None}
          .map(_.get)
      }

      def updateMem(from:state)(src:state):Unit = {
        mem.put(src, (mem.getOrElse(src, Int.MaxValue) min mem(from)) + 1)
      }
      def f(l:List[state]):Unit = l match {
        case Nil => {}
        case h::t => find(h) match {
          case Nil => f(t)
          case x::xs =>
            val not_visited = x::xs filterNot mem.contains
            not_visited foreach updateMem(h)
            f(not_visited ++ t )
        }
      }
      f(start::Nil)
      for{
        (k, v) <- mem
      } println(k, v)

      mem.getOrElse(finish, -1)
    }
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0,0,0,0,0,1),
      Array(1,1,0,0,1,0),
      Array(0,0,0,0,1,1),
      Array(0,0,1,0,1,0),
      Array(0,1,1,0,0,0),
      Array(0,1,1,0,0,0)
    )
    val grid2 = Array(Array(0,0,1,1,1,1),
      Array(0,0,0,0,1,1),
      Array(1,1,0,0,0,1),
      Array(1,1,1,0,0,1),
      Array(1,1,1,0,0,1),
      Array(1,1,1,0,0,0)
    )
    println(Solution.minimumMoves(grid))
    println(Solution.minimumMoves(grid2))
  }
}
