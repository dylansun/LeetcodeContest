object B20 {

  def f(x:Int, acc:Int = 0):Int = x match {
    case 0 => acc
    case _ => f(x >> 1, acc + (x & 1))
  }
  def sortByBits(arr: Array[Int]): Array[Int] = {
    arr.sortBy{x => (f(x), x)}
  }

  class Cashier(_n: Int, d: Int, _products: Array[Int], _prices: Array[Int]) {
    val table = scala.collection.mutable.HashMap[Int, Int]()
    (_products zip _prices) foreach {case (a, b) => table.put(a,b)}
    var x = 0
    def getBill(product: Array[Int], amount: Array[Int]): Double = {
      val ret = (product zip amount).map{case (x, y) => table(x) * y}.sum
      x +=1
      if(x == _n) {x =0; ret *(1.0 - d / 100.0)} else ret
    }

  }

  def numberOfSubstrings(s: String): Int = {
    val A = Array(0,0,0)
    val n = s.length
    def f(p1:Int, p2:Int, acc:Int = 0):Int = if(p1 == n || p2 == n) acc else {
      A(s(p2) - 'a') += 1
      if(!A.forall{x => x > 0}) f(p1, p2+1, acc)
      else {
        A(s(p1) - 'a') -= 1
        A(s(p2) - 'a') -= 1
        f(p1+1,p2, acc + (n-p2))
      }
    }
    f(0,0,0)
  }

  val mod = (1e9).toInt + 7
  def countOrders(n: Int): Int = {
    val dp = Array.fill(n+1)(BigInt(0))
    dp(1) = 1
    for{i <- 2 to n} dp(i) = dp(i-1) * i *(2*i-1) % mod
    dp.last.toInt
  }

  object P4 {
    val mod = 1e9.toInt + 7
    def f(n:Int, acc:BigInt = BigInt(1)):Int = n match {
      case 1 => acc.toInt
      case _ => f(n-1, acc*n*(2*n-1) % mod)
    }
    def countOrders(n: Int): Int = f(n)
  }

  def main(args: Array[String]): Unit = {
    println(1e9)
  }
}
