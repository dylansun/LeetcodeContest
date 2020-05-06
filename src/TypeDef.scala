object TypeDef {
  import language.postfixOps
  import scala.collection.mutable.{HashMap => HM}
  type S = String
  type C = Char
  type LT = List[Int]
  type LLT = List[List[Int]]
  type LD = List[Double]
  type LLD = List[List[Double]]
  type AT = Array[Int]
  type AAT = Array[Array[Int]]
  type LN = ListNode
  type TN = TreeNode

  val cacher = new Cache[Int, Int]()
  def f(x:Int):Int = {
    println("call f:", x)
    if(x==0) 0
    else cacher.cache(f, x-1) + x
  }
  def main(args: Array[String]): Unit = {
    println("*"*100)
    for{x <- 1 to 10} println(f(x))

    println("*"*100)
    println(cacher.mem)
  }

}
