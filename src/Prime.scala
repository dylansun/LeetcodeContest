import language.postfixOps
object Prime {
  def isPrime(x:Int):Boolean = {
    if(x <= 0) {println("negative input!"); return false}
    x match {
      case 1 => false
      case 2 => true
      case 3 => true
      case _ => for{i <- 2 to math.sqrt(x).toInt} if(x % i == 0) return false
        true
    }
  }

  def primesTable(n:Int):Array[Boolean] = {
    val ret = Array.fill(n+1)(true)
    ret(0)=false; ret(1) = false
    for {i <- 2 to math.sqrt(n).toInt}
      if(ret(i) && isPrime(i)) for{k <- 2 to (n/i)} ret(i*k) = false
    ret
  }

  def primesList(n:Int):List[Int] =
    primesTable(n).zipWithIndex filter{case (b, i) => b} map (_._2) toList
}
