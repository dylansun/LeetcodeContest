import language.postfixOps
import scala.collection.mutable.{HashMap => HM}
object C188 {
  def buildArray(target: Array[Int], n: Int): List[String] = {
    def f(x:Int):List[String] = if(target.contains(x)) List("Push") else List("Push", "Pop")
    (1 to target.max).toList flatMap f
  }

  def countTriplets(arr: Array[Int]): Int = {
    val cache = new Cache[Int,Int]()
    def cxor(i:Int):Int = if(i==0) arr.head else arr(i) ^ cache.cache(cxor, i-1)
    def rxor(i:Int, j:Int):Int = if(i==0) cache.cache(cxor, j) else cache.cache(cxor,j) ^ cache.cache(cxor, i-1)
    (for{
      i <- arr.indices
      j <- i+1 until arr.length
      k <- j until arr.length
      if rxor(i,j-1) == rxor(j,k)
    } yield 1).sum
  }


  // Todo: MLE
  def minTime(n: Int, edges: Array[Array[Int]], hasApple: List[Boolean]): Int = {
    val graph = new Graph[Int]()
    val parent = HM[Int, Int]()
    val weight = Array.fill(n)(0)
    edges foreach {case Array(a,b) => graph.add(a,b); parent.put(b, a)}
    def f(i:Int):Unit = if(parent.contains(i)) {weight(parent(i)) += 1; f(parent(i))}
    hasApple.indices foreach {i => if(hasApple(i)) {weight(i)+=1;f(i)}}
    var ret = 0
    def dfs(x:Int):Unit = for{child <- graph.get(x) if weight(child) > 0} {ret +=2; dfs(child)}
    dfs(0);ret
  }


  def ways(pizza: Array[String], K: Int): Int = {
    val row = pizza.length
    val col = pizza.head.length
    val dp = Array.fill(K, row, col)(0)
    val mat = new Matrix(pizza.map{str => str.map{case 'A' => 1
    case _ => 0}.toArray})

    dp(0)(0)(0) = 1
    for{
      k <- 1 until K
      r <- 0 until row
      c <- 0 until col
    } {
      dp(k)(r)(c) = SafeCal.sum(
        for{
          i <- 0 until r
          if mat.subMatSum(i,c, r-1, col-1) > 0
        } yield dp(k-1)(i)(c)
      )

      dp(k)(r)(c) =SafeCal.add(dp(k)(r)(c),SafeCal.sum(
        for{
          j <- 0 until c
          if mat.subMatSum(r,j,row-1, c-1) > 0
        } yield dp(k-1)(r)(j)))
    }
    SafeCal.sum(
      for{i <- 0 until row
          j <- 0 until col
          if mat.subMatSum(i,j,row-1,col-1)
      } yield dp.last(i)(j))
  }
}
