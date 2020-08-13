import scala.math
object C177 {
  object P1 {
    case class Date(y:Int, m:Int, d:Int)
    def f(s:String):Date = {
      Date(s.slice(0,4).toInt, s.slice(5,7).toInt, s.slice(8,10).toInt)
    }

    val A = Array(0,31,28,31,30,31,30,31,31,30,31,30,31)
    val B = Array(0,31,29,31,30,31,30,31,31,30,31,30,31)
    def isB(x:Int):Boolean = {
      if(x % 100 == 0) x % 400 == 0
      else x % 4== 0
    }
    // 1971-01-01 ~ yyyy - mm - dd
    // 1971 unitl yyyy
    // yyyy - 01 - 01 to mm - dd
    def g(date:Date):Int = {
      (for{i <- 1971 until date.y} yield if(isB(i)) 366 else 365).sum +
        (for{i <- 0 until date.m} yield if(isB(date.y)) B(i) else A(i)).sum +
        date.d
    }
    def daysBetweenDates(date1: String, date2: String): Int = {
      (g(f(date1)), g(f(date2))) match {
        case (a, b) => (a max b) - (a min b)
      }
    }
  }

  def validateBinaryTreeNodes(n: Int, left: Array[Int], right: Array[Int]): Boolean = {
    val A = left ++ right filter {x => x != -1}
    A.length == A.distinct.length && A.length == n-1
  }

  object P3 {
    def f(x:Int):Array[Int] = {
      val sq = math.sqrt(x).toInt
      val ans = for{i <- 1 to sq; if x % i == 0} yield Array(i, x / i)
      ans.sortBy{case Array(a, b) => math.abs(a-b)}.head
    }
    def closestDivisors(x: Int): Array[Int] = {
      List(f(x+1), f(x+2)).sortBy{case Array(a, b) => math.abs(a-b)}.head
    }
  }

  object P4 {

    def largestMultipleOfThree(digits: Array[Int]): String = {

      def f(A:Array[Int]):String = if(A.isEmpty) "" else {
        val x = A.sorted.reverse.mkString
        if(x(0) == '0') "0" else x
      }
      digits.sum % 3 match {
        case 0 => f(digits)
        case 1 =>
          digits.filter{x => x % 3 == 1}.sorted.toList match {
            case Nil => digits.filter{x => x % 3 == 2}.sorted.toList match {
              case Nil => ""
              case h::Nil => ""
              case h::h2::t => f(digits.filter{x => x % 3 != 2} ++ t)
            }
            case h::t => f(digits.filter{x => x % 3 != 1} ++ t)
          }
        case 2 =>
          digits.filter{x => x % 3 == 2}.sorted.toList match {
            case Nil => digits.filter{x => x % 3 == 1}.sorted.toList match {
              case Nil => ""
              case h::Nil => ""
              case h::h2::t => f(digits.filter{x => x % 3 != 1} ++ t)
            }
            case h::t => f(digits.filter{x => x % 3 != 2} ++ t)
          }
      }
    }
  }
}
