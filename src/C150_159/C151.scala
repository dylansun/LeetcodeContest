object C151 {
  object P1 {
    def invalidTransactions(A: Array[String]): List[String] = {
      def f(A: Array[(String, Int, Int, String)]): Array[(String, Int, Int, String)] = {
        val valid = Array.fill(A.length)(true)
        for {i <- A.indices} {
          if (A(i)._3 > 1000) {
            valid(i) = false
          }
          for {j <- i + 1 until A.length} {
            if (A(i)._4 != A(j)._4 && A(j)._2 - A(i)._2 <= 60) {
              valid(i) = false
              valid(j) = false
            }
          }
        }
        A.indices.toArray filterNot valid map A
      }
      A.map { str => str.split(",").toList match {
        case a :: b :: c :: d :: Nil => (a, b.toInt, c.toInt, d)
      }
      }
        .sortBy { case (a, b, c, d) => b }
        .groupBy { case (a, b, c, d) => a }.values.toList
        .flatMap { x => f(x.toArray) }
        .map { case (a, b, c, d) => a + "," + b + "," + c + "," + d }
    }
  }
  object P2 {
    def numSmallerByFrequency(queries: Array[String], words: Array[String]): Array[Int] = {
      def f(s: String): Int = {
        s.sorted.toList match {
          case Nil => 0
          case h :: t => g(h, t, 1)
        }
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
    def removeZeroSumSublists(head: ListNode): ListNode = {
      def f(head: ListNode, acc: List[Int]): List[Int] = head match {
        case null => acc.reverse
        case _ => f(head.next, head.x :: acc)
      }
      def solve(A: Array[Int]): Array[Int] = {
        if (A.length == 0) return A
        val cum = Array.fill(A.length)(0)
        cum(0) = A(0)
        for {i <- cum.indices.tail}
          cum(i) = cum(i - 1) + A(i)

        cum.indices foreach { i => if (cum(i) == 0) return solve(A.slice(i + 1, A.length)) }

        for {
          i <- cum.indices
          j <- i + 1 until cum.length
          if cum(j) - cum(i) == 0
        } return solve(A.slice(0, i + 1) ++ A.slice(j + 1, A.length))

        A
      }

      def construct(l: List[Int], head: ListNode): Unit = l match {
        case Nil => {}
        case h :: t =>
          val next = ListNode(h)
          head.next = next
          construct(t, head.next)
      }
      val A = solve(f(head, Nil).toArray)
      if (A.isEmpty) null
      else {
        val head = ListNode(A.head)
        construct(A.tail.toList, head)
        head
      }

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
