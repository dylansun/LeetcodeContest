package B10_19

/**
  * Created by lilisun on 10/20/19.
  */
object B11 {

  object  P1 {
    def missingNumber(A: Array[Int]): Int = {
      val d = (A.last - A.head) / A.length
      if(d == 0) return A.head
      def f(l:List[Int]):Int = l match {
        case Nil => -1
        case h1::h2::t => if(h2!=h1+d) h1+d else f(h2::t)
      }
      f(A.toList)
    }
  }

  object P2 {
    type T = Array[Int]
    def minAvailableDuration(slots1: Array[T], slots2: Array[T], duration: Int): List[Int] = {
      def f(l1:List[T], l2:List[T]):List[Int] = (l1, l2) match {
        case (Nil, _) => List.empty[Int]
        case (_, Nil) => List.empty[Int]
        case (Array(a, b)::t1, Array(c,d)::t2) =>
          if(b <= c) f(t1, l2)
          else if(d <= a) f(l1, t2)
          else {
            if((d min b) - (a max c) >= duration) List(a max c, (a max c) + duration)
            else {
              if(b > d) f(l1, t2)
              else if(b < d) f(t1, l2)
              else f(t1, t2)
            }
          }
      }
      def g(A:Array[T]):List[T] = {
        A.toList
          .filter{case Array(a, b) => b - a >= duration}
          .sortBy{case Array(a, b) => a}
      }
      f(g(slots1), g(slots2))
    }
  }
  /*
    This problem can be solved using a dynamic programming approach.
    Our DP array is d[i][j] which answers the question => What is the probability of having j items till i-th coin inclusively?
    1.

    The base case is when we have a zero target.
    This can be computed using the recurrence relation as the cumulative product of prob[i] negations.
    2.
    For all other variations of filling d[i][j], we have two options.

    i-th coin has a tail
    P1 = (proability of having j coins till i) * (probability of having a tail)
    i-th coin has a head
    P2 = (probability of having j - 1 coins till i) * (probability of having a head)
    Therefore, the probability of having j heads till i-th coin inclusively will be

    P = P1 + P2;
   */
  object P3{
    def probabilityOfHeads(prob: Array[Double], target: Int): Double = {
      val n = prob.length
      val dp = Array.fill(n+1, target+1)(0.0)
      dp(0)(0) = 1.0
      dp(1)(0) = 1-prob(0)
      for{
        i <- 1 to n
        j <- 0 to target

      }{
        if(j == 0)
          dp(i)(j) = dp(i-1)(j) * (1 - prob(i-1))
        else
          dp(i)(j) = dp(i-1)(j-1) * prob(i-1) + dp(i-1)(j) * (1-prob(i-1))
      }
      dp(n)(target)
    }
  }

  object P4 {
    def maximizeSweetness(A: Array[Int], K: Int): Int = {
      def find(l:Int, r:Int, acc:Int):Int = if(l>r) acc else {
        val mid = (l + r) / 2
        if(check(mid)) find(mid+1, r, mid)
        else find(l, mid-1, acc)
      }

      def check(bound:Int):Boolean = g(A.toList,0, 0, bound)
      def g(l:List[Int],acc:Int, cnt:Int, bound:Int):Boolean =
        if(cnt>= K+1) true else l match {
            case Nil => false
            case h::t => if(h + acc >= bound) g(t, 0, cnt+ 1, bound)
            else g(t, acc+h, cnt, bound)
          }
      find(A.min, A.sum / (K+1), A.min)
    }
  }
}
