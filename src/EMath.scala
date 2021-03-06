/**
  * common math function used in contest
  */
object EMath {
  def gcd(x:Int, y:Int):Int= if(x % y == 0) y else gcd(y, x % y)
  def abs(x:Int):Int = math.abs(x)
  def abs(x:Double):Double = math.abs(x)
  def abs(x:Long):Long = math.abs(x)
  def abs(x:Float):Float = math.abs(x)
  def lcm(x:Int, y:Int):Int = x / gcd(x, y) * y
  def catalan(n:Int):Int = catalan(1, 1, n)
  def catalan(x:Int, i:Int, n:Int):Int =
    if(i==n) x else catalan(x * (4 * i+2)/(i+2), i+1, n)
  def hasBitK(x:Int, k:Int):Boolean = (x & (1 << k)) != 0

  def main(args: Array[String]): Unit = {
    for{i <- 1 to 4} println(catalan(i))
  }
}
