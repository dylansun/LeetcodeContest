/**
  * Created by lilisun on 6/30/19.
  */
object C143 {
  case class Expr(x:Boolean, op:String, flag:Boolean)
  def parseBoolExpr(expression: String): Boolean = {
    def solve(l:List[Expr]):List[Expr] = {
      var nl = l
      var acc = List.empty[Expr]
      while(!nl.head.flag){
        acc ::= nl.head
        nl = nl.tail
      }
      nl.head.op match {
        case "Or" => Expr(acc.map(_.x).reduce(_|_),"",false)::nl.tail
        case "And" => Expr(acc.map(_.x).reduce(_&&_),"",false)::nl.tail
        case "Not" => Expr(!acc.head.x, "",false)::nl.tail
      }
    }
    def f(l:List[Char], stack:List[Expr]):Boolean = l match {
      case ','::t => f(t, stack)
      case '|'::t => f(t, Expr(true, "Or", true)::stack)
      case '!'::t => f(t, Expr(true, "Not", true)::stack)
      case '&'::t => f(t, Expr(true, "And", true)::stack)
      case '('::t => f(t, stack)
      case ')'::t => f(t, solve(stack))
      case 'f'::t => f(t, Expr(false, "", false)::stack)
      case 't'::t => f(t, Expr(true, "", false)::stack)
      case h::t => println(h::t); false
      case Nil => stack.head.x
    }

    f(expression.toList,Nil)
  }
  def pathInZigZagTree(label: Int): List[Int] = {
    def pow2(x:Int):Int = Math.pow(2,x).toInt
    def g(x:Int, i:Int):Int =  i % 2 match {
      // use Math to solve
      // i-th layer: 2^(i-1), 2^i -1
      // first a0 = 2^i -1 , d = -1, a0+n*d = x => n = a0 - x
      case 0 => (pow2(i)-1 - x) / 2 + pow2(i-2)
      // first a0 = 2^(i-1), d = 1, a0 + n = x => n = x - a0
      case 1 => pow2(i-1) - 1 - (x - pow2(i-1))/2
    }

    def log2(x:Int, acc:Int = 0):Int = x match {
      case 0 => acc
      case _ => log2(x / 2, acc + 1)
    }

    def f(x:Int, acc:List[Int]):List[Int] = x match {
      case 1 => 1::acc
      case _ => f(g(x, log2(x)),x::acc)
    }
    f(label,Nil)
  }
  def minHeightShelves(A: Array[Array[Int]], w: Int): Int = {
    val dp = Array.fill(A.length+1)(Int.MaxValue)
    dp(0) = 0
    for{i <- 0 until A.length}{
      var width = 0
      var height = 0
      for{j <- i until A.length}{
        width+= A(j)(0)
        height = height max A(j)(1)
        if(width <= w)
          dp(j+1) = dp(j+1) min (dp(i)+ height)
      }
    }
    dp.last
  }
}
