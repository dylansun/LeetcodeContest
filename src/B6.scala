/**
  * Created by lilisun on 8/10/19.
  */
object B6 {
  object P4 {
    def canConvert(str1: String, str2: String): Boolean = {
      val table = scala.collection.mutable.HashMap[Char, Set[Char]]()
      (str1 zip str2) foreach {case (x, y) => table.put(y,table.getOrElse(y, Set.empty[Char]) + x)}
      for{
        x <- 'a' to 'z'
        y <- 'a' to 'z'
        if x != y
        if table.contains(x)
        if table.contains(y)
        if (table(x) & table(y)).nonEmpty
      } return false

      if(table.keySet.size == 26) table.keySet.forall(key => table(key).size == 1 && table(key).head == key)
      else true
    }
  }
  object P3 {
    case class Pattern(l:List[String]){
      def min(that:Pattern):Pattern = {
        l zip that.l dropWhile {case (a, b) => a == b} match {
          case Nil => this
          case (a,b)::t => if(a < b) this else that
        }
      }
    }
    def mostVisitedPattern(u: Array[String], time: Array[Int], site: Array[String]): List[String] = {
      val fre = scala.collection.mutable.HashMap[Pattern, Int]()
      def f(l:List[String]):Unit = {
        val seq = l.toArray
        var xs = List.empty[Pattern]
        for{
          i <- 0 until seq.length
          j <- i+1 until seq.length
          k <- j+1 until seq.length
          pattern = Pattern(seq(i)::seq(j)::seq(k)::Nil)
        } if(!xs.contains(pattern)){
          fre.put(pattern, 1 + fre.getOrElse(pattern, 0))
          xs ::= pattern
        }
      }

      val n = u.length
      val log = (0 until n).toArray.map{i => (u(i), time(i), site(i))}
      log
        .groupBy{case (x,y,z) => x}.values.toList
        .map{x => x.sortBy{case (x,y,z) => y}}
        .map{x => x.map{case (x,y,z) => z}.toList} foreach f

      def findMaxPattern(l:List[Pattern], n:Int = 0, p:Pattern = Pattern(Nil)):Pattern = l match {
        case Nil => p
        case h::t =>
          if(fre(h) > n) findMaxPattern(t, fre(h), h)
          else if(fre(h) == n) findMaxPattern(t, fre(h), h min p)
          else findMaxPattern(t, n, p)
      }

      findMaxPattern(fre.keySet.toList).l
    }
  }

  object P2 {
    def cum(A:Array[Int]):Array[Int] = {
      val B = Array.fill(A.length)(0)
      for{i <- B.indices} if(i == 0) B(i) = A(i) else B(i) = B(i-1) + A(i)
      B
    }
    def minSwaps(A: Array[Int]): Int = {
      val n = A.length
      val m = A.sum
      if(m == 0) return 0
      val B = cum(A)
      def f(i:Int, acc:Int):Int = {
        if(i + m >= n) acc
        else f(i+1, acc max (B(i+m) - B(i)))
      }
      m - f(0,B(m-1))
    }
  }

  object P1 {
    def isMajorityElement(A: Array[Int], x: Int): Boolean = {
      A.count(_==x) > (A.length / 2)
    }
  }
}
