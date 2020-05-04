/**
  * Created by lilisun on 5/2/20.
  */
object C181 {
  object P1 {
    def createTargetArray(nums: Array[Int], index: Array[Int]): Array[Int] = {
      def f(l: List[(Int, Int)], acc: List[(Int, Int)]): Array[Int] = l match {
        case Nil => acc.sortBy { case (_, x) => x }.map(_._1).toArray
        case (x, i) :: t => acc partition { case (y, j) => j < i } match {
          case (l1, l2) => f(t, l1 ++ ((x, i) :: g(i, l2)))
        }
      }

      def g(i: Int, l: List[(Int, Int)]): List[(Int, Int)] = l match {
        case Nil => Nil
        case (x, j) :: t => if (i == j) (x, j + 1) :: g(i + 1, t) else l
      }

      f((nums zip index).toList, Nil)
    }

  }

  object p2 {
    val pl = Prime.primesList(100000)
    val pt = Prime.primesTable(100000)

    def sumFourDivisors(nums: Array[Int]): Int = {
      def f(x: Int, l: List[Int]): Int = l match {
        case Nil => 0
        case h :: t =>
          if (x % h == 0)
            if (h <= math.sqrt(x) && (pt(x / h) || x == h * h * h) && x != h * h) 1 + x + h + x / h else 0
          else f(x, t)
      }
      nums map { x => f(x, pl) } sum
    }
  }

  object P3{
    def hasValidPath(grid: Array[Array[Int]]): Boolean = {
      val n = grid.length
      val m = grid(0).length
      val dsu = new DSU(n*m)
      def getId(x:Int,y:Int):Int = x*m+y
      def detectL(x:Int,y:Int):Unit = {if(y-1>=0 && Set(1,4,6).contains(grid(x)(y-1))) dsu.union(getId(x,y), getId(x, y-1))}
      def detectR(x:Int,y:Int):Unit = {if(y+1<m && Set(1,3,5).contains(grid(x)(y+1))) dsu.union(getId(x,y), getId(x,y+1))}
      def detectU(x:Int,y:Int):Unit = {if(x-1>=0 && Set(2,3,4).contains(grid(x-1)(y))) dsu.union(getId(x,y), getId(x-1,y))}
      def detectD(x:Int,y:Int):Unit = {if(x+1<n && Set(2,5,6).contains(grid(x+1)(y))) dsu.union(getId(x,y), getId(x+1,y))}
      def handler(x:Int,y:Int):Unit = grid(x)(y) match {
        case 1 => detectL(x,y); detectR(x,y)
        case 2 => detectU(x,y); detectD(x,y)
        case 3 => detectL(x,y); detectD(x,y)
        case 4 => detectR(x,y); detectD(x,y)
        case 5 => detectL(x,y); detectU(x,y)
        case 6 => detectU(x,y); detectR(x,y)
      }
      for{i <- grid.indices; j<- grid(i).indices} handler(i,j)
      dsu.find(getId(0,0)) == dsu.find(getId(n-1,m-1))
    }
  }


  def main(args: Array[String]): Unit = {
    println(Prime.primesList(100000).length)
  }
}
