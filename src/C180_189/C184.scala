import scala.collection.mutable.{ArrayBuffer =>AB}
object C184 {
  object P1 {
    def stringMatching(words: Array[String]): List[String] = {
      (for{
        i <- words.indices
        if words.indices.filter(_!=i).exists(j => words(j).contains(words(i)))
      } yield words(i)).toList
    }
  }
  object P2 {
    def processQueries(queries: Array[Int], m: Int): Array[Int] = {
      var A=AB[Int]()
      val ret = AB[Int]()
      for {x <- 1 to m} A.append(x)
      for{x <- queries}{
        ret.append(A.indexOf(x))
        A.remove(ret.last)
        A.insert(0, x)
      }
      ret.toArray
    }
  }

  object P3 {
    case class P(p:String, q:String)
    val l = List(P("&quot;", "\""),P("&apos;", "'"), P("&gt;", ">"),
      P("&lt;", "<"), P("&frasl;", "/"), P("&amp;","&"))
    def entityParser(text: String): String = solve(text, l)
    def solve(text:String, l:List[P]):String = l match {
      case Nil => text
      case P(p,q)::t => solve(text.replaceAll(p,q), t)
    }
  }
  object P4 {
    case class E22(a0:Int, b0:Int, a1:Int, b1:Int)
    case class E12(a0:Int, a1:Int){
      def *(that:E22):E12 =
        E12(SafeCal.add( SafeCal.mul(a0, that.a0), SafeCal.mul(a1, that.b0)),
          SafeCal.add( SafeCal.mul(a0, that.a1), SafeCal.mul(a1, that.b1)))
      def sum():Int = SafeCal.add(a0, a1)
    }
    def numOfWays(n: Int, e12:E12 = E12(6,6)): Int = n match {
      case 1 =>e12.sum
      case _ => numOfWays(n-1, e12 * E22(3,2,2,2))
    }
  }
  def main(args: Array[String]): Unit = {

  }
}
