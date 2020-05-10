class Matrix(mat:Array[Array[Int]]) {
  val row = mat.length
  val col = mat(0).length
  val rowsum = Array.fill(row, col)(0)
  for{r <- 0 until row; c <- 0 until col} {
    if (c == 0) rowsum(r)(c) = mat(r)(c)
    else rowsum(r)(c) = mat(r)(c) + rowsum(r)(c - 1)
  }
  def subMatSum(x0:Int,y0:Int,x1:Int,y1:Int):Int = {
    (for{i <- x0 to x1} yield
      if(y0==0) rowsum(i)(y1)
      else rowsum(i)(y1) - rowsum(i)(y0-1)
      ).sum
  }
}
