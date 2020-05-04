/**
  * Created by lilisun on 3/24/20.
  */
object NQueen {
  type Queen = (Int, Int)
  type T = List[List[Queen]]
  def placeQueens(size:Int)(n: Int): T = n match {
    case 0 => List(Nil)
    case _ =>
      for {
        queens <- placeQueens(size)(n -1)
        y <- 1 to size
        queen = (n, y)
        if isSafe(queen, queens)
      } yield queen :: queens
  }
  def isSafe(queen: Queen, others: List[Queen]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: Queen, q2: Queen) =
    q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q2._1-q1._1).abs == (q2._2-q1._2).abs

  def solveNQueens(n: Int): List[List[String]] = {
    placeQueens(n)(n) map {l => l map {case (r,c) =>
      (for{i <- 1 to n} yield if(i==c) "Q" else ".").mkString
    }}
  }
}
