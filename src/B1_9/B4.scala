package B1_9

/**
  * Created by lilisun on 7/17/19.
  */
object B4 {
  def longestWPI(hours: Array[Int]): Int = {
    val A = hours.map(x => if(x > 8) 1 else -1)
    val B = Array.fill(A.length)(0)
    B(0) = A(0)
    A.indices.tail foreach {i => B(i) = B(i-1) +A(i)}
    // println(B.toList)
    var max_len = B.indices.reverse.dropWhile(i => B(i) < 0).headOption.getOrElse(-1) + 1
    //B.indices foreach {i => if(B(i) > 0) max_len = i+1}
    // println(max_len)
    for{
      i <- B.indices
      j <- i+1 until B.length
      if B(j) - B(i) > 0
    } max_len = max_len max (j-i)
    max_len
  }

}
