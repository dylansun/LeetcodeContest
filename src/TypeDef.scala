object TypeDef {
  import language.postfixOps
  import scala.collection.mutable.{HashMap => HM}
  type S = String
  type C = Char
  type LC = List[Char]
  type LT = List[Int]
  type LLT = List[List[Int]]
  type LD = List[Double]
  type LLD = List[List[Double]]
  type AT = Array[Int]
  type AAT = Array[Array[Int]]
  type LN = ListNode
  type TN = TreeNode

  def main(args: Array[String]): Unit = {

    val cf = new Cache[Int, Int]().func _
    def f(x:Int):Int = {
      println(s"called: f($x)")
      if(x<=0) 0 else cf(f)(x-1) + x
    }
    for {x <- 50 to 1 by -1} println(x, cf(f)(x))

  }

}
