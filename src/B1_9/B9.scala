import scala.collection.mutable.PriorityQueue
object B9 {
  object P1 {
    def maxNumberOfApples(A: Array[Int]): Int = {
      val B = A.sorted
      var cnt = 0
      var mount = 0
      B foreach {x =>
        if(mount + x > 5000) return cnt
        else{
          mount += x
          cnt += 1
        }
      }
      cnt
    }
  }

  object P2{
    def minKnightMoves(x:Int, y:Int):Int = {
      def f(x:Int, y:Int):Int = (x, y) match {
        case (1, 1) => 2
        case (2, 2) => 4
        case (1, 0) => 3
        case (a, b) if a*2 == b =>  (a + b) / 3
        case (a, b) if a*2 >= b => (x + y) / 3 + (x + y) % 3
        case (a, b) =>
          var ans = a
          val c = (b - 2 * a) % 4
          ans += c
          ans += (y - 2 * a - c) / 2
          ans
      }

      f(math.abs(x) min math.abs(y), math.abs(x) max math.abs(y))
    }
  }

  object P3 {
    type LL = List[List[Int]]
    def smallestCommonElement(mat: Array[Array[Int]]): Int = {
      def f(ll:LL):Int = {
        if(ll.contains(List.empty[Int])) -1
        else {
          val m = ll.map{case h::t => h}.max
          if(ll forall {case h::t => h == m}) m
          else f(ll.map{case h::t => if(h == m) h::t else t})
        }
      }
      f(mat.map(_.toList).toList)
    }
  }

  def minBuildTime(blocks: Array[Int], split: Int): Int = {
    val pq = PriorityQueue[Int]()(Ordering.by(x => -x))
    blocks foreach {x => pq.enqueue(x)}
    while(pq.size > 1) {pq.enqueue((pq.dequeue() max pq.dequeue()) + split)}
    pq.dequeue()
  }

}
