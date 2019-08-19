/**
  * Created by lilisun on 6/15/19.
  */
import scala.collection.mutable
object B2 {


  def highFive(items: Array[Array[Int]]): Array[Array[Int]] = {

    def f(sa:Array[Array[Int]]):Array[Int] = {
      val avg = sa.sortBy{case Array(id, score) => - score}.slice(0,5)
          .map{case Array(id, score) => score}
          .sum / 5
      Array(sa.head.head, avg)
    }
    items
      .groupBy{case Array(id, score) => id}
      .values
      .toArray
      .map(f)
  }

  case class Elem(s:String, l:List[mutable.Set[Char]])
  def f(elem:Elem):List[Elem] = elem.l match {
    case Nil => List(elem)
    case h::t => h.toList.map{ch => Elem(elem.s + ch.toString, t)}
  }
  def solve(l:List[Elem]):List[Elem] = l.head.l match {
    case Nil => l
    case h::t => solve(l flatMap f)
  }
  def permute(S: String): Array[String] = {
    solve(List(Elem("",parse(S.replaceAll(",", ""), Nil))))
      .toArray
      .map{case Elem(str, _) => str}.sorted
  }

  def parse(str:String, acc:List[mutable.Set[Char]]):List[mutable.Set[Char]] = {
    if (str == "") acc.reverse
    else str.head match {
      case '{' =>
        val elem = mutable.Set.empty[Char]
        var tmp = str.tail
        while(tmp.head != '}'){
          elem += tmp.head
          tmp = tmp.tail
        }
        parse(tmp.tail, elem::acc)
      case ',' => println("comma");parse(str.tail, acc)
      case _ => parse(str.tail, mutable.Set(str.head)::acc)
    }
  }

  def confusingNumberII(N: Int): Int = {
    println((1 to N).toList.filter(x => isConfusing(x)))
    (1 to N).count(x => isConfusing(x))
  }

  // 234
  // 2 -> 0 ??
  //      1 ??
  //      2 0?
  //      2 1?
  //      2 2?
  //      2 3?
  //      2 30
  //....
  def isConfusing(N:Int):Boolean = {
    val l = N.toString.toArray.toList.map{ch => ch - '0'}
    (l forall {x => List(0,1,6,8,9).contains(x)}) && (l != l.reverse.map{
      case 9 => 6
      case 6 => 9
      case x => x
    })
  }


  def sumOfDigits(A: Array[Int]): Int = {
    (A.min.toString.toArray.map{x => x - '0'}.sum + 1)% 2
  }

  def main(args: Array[String]): Unit = {
      println(confusingNumberII(20))
  }
}
