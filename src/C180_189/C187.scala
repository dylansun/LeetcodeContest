object C187 {
  def destCity(paths: List[List[String]]): String = {
    val src = paths.map(_.head)
    (paths map {case List(a,b) => b} filterNot src.contains).head

  }

  def kLengthApart(nums: Array[Int], k: Int): Boolean = {
    def f(l:List[Int]):Boolean = l match {
      case Nil => true
      case h::Nil => true
      case i::j::t => if(j-i>=k+1) f(l.tail) else false
    }
    f(nums.zipWithIndex.filter(_._1==1).map(_._2).toList)
  }
}
