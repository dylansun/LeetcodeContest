object C151 {
  object P1 {
    case class Elem(name:String, time:Int, fee:Int, city:String){
      def inValid():Boolean = fee > 1000
      def inValid(that:Elem):Boolean = {
          this.name == that.name &&
          math.abs(this.time - that.time) <= 60 &&
          this.city != that.city
      }
      def get():String = List(name, time.toString, fee.toString, city).mkString(",")
    }
    def trans (str:String):Elem = str.split(",").toList match {
      case a :: b :: c :: d :: Nil => Elem(a, b.toInt, c.toInt, d)
    }
    def f(A: Array[Elem]): Array[Elem] = A filter {elem => elem.inValid || A.exists(x => x inValid elem)}
    def invalidTransactions(A: Array[String]): List[String] = {
      f(A map trans).map(_.get()).toList
    }
  }
  object P2 {
    def numSmallerByFrequency(queries: Array[String], words: Array[String]): Array[Int] = {
      def f(s: String): Int = s.sorted.toList match {
        case Nil => 0
        case h :: t => g(h, t, 1)
      }

      def g(x: Char, l: List[Char], acc: Int): Int = l match {
        case h :: t => if (h == x) g(x, t, acc + 1) else acc
        case Nil => acc
      }
      val B = words map f
      queries map f map { x => B.count(_ > x) }
    }
  }
  object P3 {
    def ln2l(head: ListNode, acc: List[Int]): List[Int] = head match {
      case null => acc.reverse
      case _ => ln2l(head.next, head.x :: acc)
    }

    def l2ln(guard:ListNode)(l: List[Int], head: ListNode): ListNode = l match {
      case Nil => guard.next
      case h :: t =>
        val next = new ListNode(h)
        head match {
          case null => guard.next = next
          case _ => head.next = next
        }
        l2ln(guard)(t, next)
    }
    def removeZeroSumSublists(head: ListNode): ListNode = {

      def solve(A: Array[Int]): Array[Int] = {
        var rmd = List.empty[Int]
        val idx = scala.collection.mutable.HashMap[Int, Int]()
        if(A.length == 0) return A
        val cum = Array.fill(A.length)(0)
        cum(0) = A(0)
        if(cum(0) == 0) rmd ::= 0 else idx.put(cum(0), 0)
        cum.indices.tail foreach { i =>
          cum(i) = cum(i-1) +A(i)
          if(cum(i) == 0) rmd = (0 to i).toList
          else if(idx.contains(cum(i)) && !rmd.contains(idx(cum(i))))
            rmd = rmd ++ (idx(cum(i))+1 to i).toList
          else idx.put(cum(i), i)

        }
        A.indices.toArray filterNot rmd.contains map A
      }

      l2ln(new ListNode(0))(solve(ln2l(head, Nil).toArray).toList, null)
    }
  }
  object P4 {
  class DinnerPlates(_n: Int) {

    val stack = scala.collection.mutable.HashMap[Int, List[Int]]()
    val n = _n

    def push(x: Int) {
      for {
        i <- 0 until 100000
        if (!stack.contains(i)) || (stack(i).length < n)
      } {
        stack.put(i, x :: stack.getOrElse(i, Nil))
        return
      }
    }

    def pop(): Int = if (stack.keySet.isEmpty) -1 else popAtStack(stack.keySet.max)

    def removeStack(i: Int): Unit = {
      if (stack.contains(i) && stack(i).isEmpty) {
        stack.remove(i)
      }
    }

    def popAtStack(index: Int): Int = {
      if (stack.contains(index) && stack(index).nonEmpty) {
        val x = stack(index).head
        stack(index) = stack(index).tail
        removeStack(index)
        x
      } else -1
    }

  }

  }
}
