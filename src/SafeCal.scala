object SafeCal {
  val mod = 1e9.toLong + 7
  val zero = 0.toLong
  def getMod(x:Long, flag:Boolean=true):Long = if(flag) x % mod else x
  def sum(seq:Seq[Int], flag:Boolean = true):Int =
    getMod(seq.foldLeft(zero){(acc, x)=> acc + x.toLong}, flag).toInt
  def mul(x:Int, y:Int, flag:Boolean = true):Int =
    getMod(x.toLong * y.toLong, flag).toInt
  def add(x:Int, y:Int, flag:Boolean = true):Int =
    getMod(x.toLong + y.toLong, flag).toInt
}
