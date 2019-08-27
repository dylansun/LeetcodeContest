package C140_149

/**
  * Created by lilisun on 7/21/19.
  */
object C146 {
  def numEquivDominoPairs(A: Array[Array[Int]]): Int = {
    A.map {case Array(x,y) => (x min y, x max y)}
      .groupBy(x => x)
      .values.toList
      .foldLeft(0){(sum , x) => sum + x.length * (x.length -1) / 2}
  }

  def main(args: Array[String]): Unit = {
    var b = BigInt(1)
    for {i <- 1 to 40} b *= 15
    println(b)
  }
}
