package C140_149

/**
  * Created by lilisun on 8/11/19.
  */
object C149 {
  object P3 {
    case class Elem(ch:Char, n:Int)
    def parser(l:List[Char], xs:List[Elem] = Nil):List[Elem] = l match {
      case Nil => xs
      case h::t => xs match {
        case Nil => parser(t, Elem(h,1)::Nil)
        case Elem(y, n)::t2 =>
          if (y == h) parser(t, Elem(y, n+1)::t2)
          else parser(t, Elem(h,1)::xs)
      }
    }

    def maxRepOpt1(text: String): Int = {
      val fre = scala.collection.mutable.HashMap[Char, Int]()
      text foreach {ch => fre.put(ch, fre.getOrElse(ch, 1) + 1)}
      val xs = parser(text.toList, Nil)
      def solve(xs:List[Elem], acc:Int):Int = xs match {
        case Elem(a,n1)::Elem(b,n2)::Elem(c,n3)::t =>
          if(n2 == 1 && a == c) {
            val n = (if(n1+n3 < fre(a)) n1+n3+1 else n1+n3)
            solve(xs.tail, acc max n)
          }else {
            val n = (if(n1 < fre(a)) n1+1 else n1)
            solve(xs.tail, acc max n)
          }
        case Elem(a,n1)::Elem(b,n2)::Nil =>
          val m = (if(fre(a) > n1) n1+1 else n1)
          val n = (if(fre(b) > n2) n2+1 else n2)
          acc max m max n
        case Elem(a,n1)::Nil => acc max n1
        case Nil => acc
      }

      solve(xs, 0)

    }
  }

  class MajorityChecker(_arr: Array[Int]) {

    val A = _arr
    def count(x:Int):Array[Int] = {
      val B = Array.fill(20001)(0)
      for{i <- 0 to x} B(A(i)) += 1
      B
    }
    val mem = scala.collection.mutable.HashMap[Int, Array[Int]]()
    def query(left: Int, right: Int, threshold: Int): Int = {
      // println(left,right,threshold)
      if(left == right && threshold == 1) return A(left)
      val ls = if(mem.contains (left)) mem(left) else count(left)
      val rs = if(mem.contains(right)) mem(right) else count(right)
      mem.put(left,ls)
      mem.put(right,rs)
      rs(A(left)) += 1
      for{i <- 1 to 20000 if(rs(i) - ls(i) >= threshold)} {rs(A(left)) -=1; return i}
      rs(A(left))-=1
      -1
    }

  }
  object P2 {
    // d, 1~f, target
    // d-1, 1~f, target - 1
    // ..
    // d-1, 1~f, target - f
    // f(target, d)= sum_i (f(target-i, d-1)), i <- 1~ f
    val mod = 1000000007
    def numRollsToTarget(d: Int, f: Int, target: Int): Int = {
      val dp = Array.fill(target + 1, d+1)(0)
      for{
        i <- 1 to target
        j <- 1 to d
      }{
        if(j == 1) {
          if((1 to f).contains(i)){
            dp(i)(j) = 1
          }
        } else
          for(k <- 1 to f if i - k >= 0){
            dp(i)(j) += dp(i-k)(j-1)
            dp(i)(j) = dp(i)(j) % mod
          }

      }
      dp(target)(d)
    }
  }

  object P1 {

    val A = Array(0,31,28,31,30,31,30,31,31,30,31,30,31)
    val B = Array(0,31,29,31,30,31,30,31,31,30,31,30,31)
    def isB(x:Int):Boolean = {
      if(x % 100 == 0) x % 400 == 0
      else x % 4== 0
    }
    def ordinalOfDate(data: String): Int = {
      val y = data.slice(0,4).toInt
      val m = data.slice(5,7).toInt
      val d = data.slice(8,10).toInt
      if(isB(y)) B.slice(0, m).sum + d
      else A.slice(0,m).sum + d

    }
  }
  def main(args: Array[String]): Unit = {
    val mc = new MajorityChecker(Array(1,1,2,2,1,1))
    println(mc.query(2,3,2))
  }
}
