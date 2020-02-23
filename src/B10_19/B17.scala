object B17 {
  def decompressRLElist(nums: Array[Int]): Array[Int] = {
    (0 until nums.length/2).toArray.flatMap{i =>
      Array.fill(nums(2*i))(nums(2*i+1))
    }
  }
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val n = mat.length
    val m = mat(0).length
    val P = Array.fill(n+1, m+1)(0) //
    for{i <- 1 to n ; j <- 1 to m}
      P(i)(j) = P(i-1)(j) + P(i)(j-1) - P(i-1)(j-1) + mat(i-1)(j-1)
    for{i<- 0 until n; j <- 0 until m}{
      val lx = 0 max (i-K)
      val ly = 0 max (j-K)
      val rx = (i+K+1) min n
      val ry = (j+K+1) min m
      mat(i)(j) = P(rx)(ry) - P(lx)(ry) - P(rx)(ly) + P(lx)(ly)
    }
    mat
  }

  type T = TreeNode
  case class Elem(n:T, p:T, pp:T){
    def getValue():Int = if(pp != null && pp.value % 2 ==0) n.value else 0
    def next():List[Elem] = n match {
      case null => Nil
      case _ => List(Elem(n.left, n, p), Elem(n.right, n, p)).filter(_.isValid)
    }
    def isValid():Boolean = n != null
  }
  def sumEvenGrandparent(root: TreeNode): Int = {
    def f(l:List[Elem], acc:Int):Int = l match {
      case Nil => acc
      case _ => f(l flatMap (_.next),
        acc + l.map(_.getValue()).sum)
    }
    f(List(Elem(root, null, null)), 0)
  }

  def distinctEchoSubstrings(text: String): Int = {
    val n = text.length
    var set = Set.empty[String]
    for{
      i <- 0 until n
      j <- i+1 until n
      if (j-i+1) % 2 == 0
      d = (j-i+1) / 2
      if text.slice(i, i+d) == text.slice(i+d, i+2*d)
    } set += text.slice(i,i+d)
    set.size
  }


  def main(args: Array[String]): Unit = {
    //[1,33,
    // [121,114,  4, 109, 44, 70, 61, 52,73, 84,  8, 28, 46,105,162],
    // [401,513,289, 761,786,767,826,904,48,360,234,536,611,872,369]]
    // [8,73,70,61],[212,450,602,564]
    val a = 234 * 212 + 48 * 450 + 767 * 602 + 826*564
    println(a, a * (1-1.0 / 3))
  }
}
