import scala.collection.mutable
import mutable.{HashMap => HM, Set => mset}
object C189 {

  object P1 {
    def busyStudent(startTime: Array[Int], endTime: Array[Int], q: Int): Int = {
      (startTime zip endTime).count{case (a, b) => a<=q && q <= b}
    }
  }

  object P2 {
    def arrangeWords(text: String): String = {
      (text.head.toLower + text.tail)
        .split(' ')
        .zipWithIndex
        .sortBy{case (x, i) => (x.length,i)}.toList.map(_._1) match {
        case Nil => ""
        case h::Nil => h.head.toUpper + h.tail
        case h::t => (h.head.toUpper + h.tail) +" " +t.mkString(" ")
      }
    }
  }
  object P3 {
    def peopleIndexes(l: List[List[String]]): List[Int] = {
      val table = HM[String, mset[Int]]()
      for{
        i <- l.indices
        company <- l(i)
      } if(!table.contains(company)) table.put(company, mset(i)) else table(company) += i

      (for{
        i <- l.indices
        if (l(i) map table reduce (_&_)). size == 1
      } yield i).toList
    }
  }

  object P4 {
    case class Point(x:Double, y:Double){
      def dis_sq(that:Point):Double =
        math.pow(x - that.x, 2) + math.pow(y - that.y, 2)
    }
    def numPoints(points: Array[Array[Int]], r: Int): Int = {
      val ps = points map {case Array(x,y) => Point(x,y)}
      (Seq(1) ++ (for{
        i <- ps.indices
        j <- ps.indices
        if ps(i) != ps(j)
        center <- cal_center(ps(i), ps(j), r)
      } yield count_point(ps,center, r*r))).max
    }
    def cal_center(p1:Point, p2:Point, r:Double):List[Point] = {
      if(p1.dis_sq(p2) > 4*r*r) Nil
      else{
        val d_sq = p1 dis_sq p2
        val k = math.sqrt(4*r*r - d_sq) / 2 / math.sqrt(d_sq)
        val Z0 = Point(
          (p1.x + p2.x) /2 + (p1.y - p2.y)*k,
          (p1.y + p2.y) /2 + (p2.x - p1.x)*k
        )
        val Z1 = Point(
          (p1.x + p2.x) /2 + (p2.y - p1.y)*k,
          (p1.y + p2.y) /2 + (p1.x - p2.x)*k
        )
        List(Z0, Z1)
      }
    }
    def count_point(points: Array[Point], center:Point, r_sq:Double):Int =
      points.count(x => x.dis_sq(center) <= r_sq)


  }
}
