import language.postfixOps
object C185 {
  import scala.collection.mutable.{HashMap => HM}

  def reformat(s: String): String = {
    def zigzag(s1:String, s2:String):String=
      (s1 zip s2).foldLeft(""){(acc, x) => acc + x._1 + x._2}
    s partition (_.isLetter) match {
      case (c, d) =>
        if (d.length == c.length) zigzag(d, c)
        else if (d.length == c.length + 1) zigzag(d.tail, c) + d.head
        else if (d.length + 1 == c.length) c.head + zigzag(d, c.tail)
        else ""
    }
  }

  def displayTable(orders: List[List[String]]): List[List[String]] = {
    val tids = orders.map{l => l.tail.head} sortBy { x => x.toInt}
    val foods =orders.map{l => l.tail.tail.head} sorted
    val freq = HM[(String,String), Int]()
    orders foreach {
      case name::tid::food::Nil =>
        freq.put((tid,food), 1+freq.getOrElse((tid,food), 0))
      case _ => "error"
    }
    ("Table"::foods)::(for{tid <- tids} yield
      tid::foods.map(food => freq.getOrElse((tid, food), 0).toString))

  }

  def minNumberOfFrogs(croakOfFrogs: String): Int = {
    val alpha = "croak".zipWithIndex.toMap
    val fre = Array.fill(5)(0)
    def f(l:List[Char], acc:Int):Int = l match {
      case Nil => if(fre.forall(_==0)) acc else -1
      case h::t => fre(alpha(h)) += 1
        if(alpha(h) == alpha.size -1)
          fre.indices foreach {i => fre(i)-=1}
        if(fre.indices.dropRight(1).exists(i => fre(i) < fre(i+1))) -1
        else if(fre.exists(_<0)) -1
        else f(t, acc max fre.max)
    }
    f(croakOfFrogs.toList, 0)
  }

  val cache = new Cache[(Int,Int,Int), Int]()
  def numOfArrays(n: Int, m: Int, k: Int): Int = {
    def dp(input:(Int,Int,Int)):Int = input match {
      case (1,1,_) => 1
      case (0,_,_) => 0
      case (_,0,_) => 0
      case (i,j,p) =>
        SafeCal.sum(for{q <- 1 until p} yield cache.cache(dp, (i-1,j-1,q)))+
        SafeCal.mul(cache.cache(dp, (i-1,j,p)),p) % (1e9.toInt +7)
    }
    SafeCal.sum(for{ x <- 1 to m} yield dp(n,k,x))
  }
}
