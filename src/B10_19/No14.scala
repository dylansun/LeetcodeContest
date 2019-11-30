package B10_19

/**
  * Created by lilisun on 12/1/19.
  */
object No14 {
  object P1 {
    val zero = BigInt(0)
    def toHexspeak(num: String): String = {
      check(toHex(BigInt(num), ""))
    }
    def check(str:String):String = {
      println(str)
      if(str.exists(ch => ('2' to '9').contains(ch))) "ERROR" else str
    }
    def toHex(x:BigInt, acc:String):String= x match {
      case a if a == zero => acc
      case _ => toHex(x / 16, f((x % 16).toInt)+acc)
    }
    def f(x:Int):String = x match {
      case 0 => "O"
      case 1 => "I"
      case _ =>
        if(x < 10) x.toString else (x - 10 + 'A').toChar.toString
    }
  }

  object P2 {
    type AI = Array[Int]
    type LLI = List[List[Int]]
    def removeInterval(A: Array[AI], r: AI): LLI = f(r)(A.toList, Nil)
    def f(r:AI)(l:List[AI], acc:LLI):LLI = l match {
      case Nil => acc filterNot {case List(a,b) => a == b} reverse
      case h::t => (h, r) match {
        case (Array(a,b),Array(c,d)) => kind(h, r) match {
          case 0 => f(r)(t, List(a,b)::acc)
          case 1 => f(r)(t, acc)
          case 2 => f(r)(t, List(d,b)::List(a,c)::acc)
          case 3 => f(r)(t, List(a, c)::acc)
          case 4 => f(r)(t, List(d, b)::acc)
        }
      }
    }

    def kind(A:AI, B:AI):Int = (A, B) match {
      case (Array(a,b), Array(c,d)) =>
        if(b<= c || a >= d) 0 // not intersect
        else if(c<=a && d >= b) 1// B contains A
        else if(a <= c && b >= d) 2 // A contains B
        else if(a <= c && b >= c) 3 // A right intersect B
        else 4 // A left interset B
    }
  }

  object P3 {
    def deleteTreeNodes(n: Int, parent: Array[Int], value: Array[Int]): Int = {
      val w = Array.fill(n)(1)
      for{j <- (0 until n).reverse}{
        if(value(j) != 0) {
          if(parent(j) == -1) return w.sum
          value(parent(j)) += value(j)
          w(parent(j)) += w(j)
        }
        w(j) = 0
      }
      w.sum
    }
  }

  // p4 not support scala
}
