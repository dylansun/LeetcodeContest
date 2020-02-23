object C176 {


  def countNegatives(grid: Array[Array[Int]]): Int = {
    grid.flatten.count(_<0)
  }


  class ProductOfNumbers() {
    var seq = Seq.empty[Int]
    def add(x: Int) {
      if(x == 0) seq = Seq.empty[Int] else {
        if(seq.isEmpty) seq = seq :+ x
        else seq = seq :+ seq.last*x
      }
    }

    def getProduct(k: Int): Int = {
      if(k>seq.length) 0
      else if(k==seq.length) seq.last
      else seq.last / seq(seq.length-k-1)
    }
  }


  def maxEvents(events: Array[Array[Int]]): Int = {
    var set = Set.empty[Int]
    def add(x:Int,end:Int):Unit = if(x<=end){
      if(set.contains(x)) add(x+1, end)
      else set += x
    }

    events sortBy {case Array(start, end) => (end, start)} foreach {
      case Array(start,end) => add(start,end)
    }
    set.size
  }


  def isPossible(A: Array[Int]): Boolean = {
    val pq = scala.collection.mutable.PriorityQueue[Int]()
    A.foreach{x => pq.enqueue(x)}
    def solve(arr_sum:Int):Boolean = {
      //   println(arr_sum, pq)
      if(arr_sum == A.length) true else {
        val q = pq.dequeue
        val p = arr_sum - q
        if(p >= q || p <= 0) false else {
          pq.enqueue(q-(q-1)/p *p)
          solve(arr_sum - (q-1) / p * p)
        }
      }
    }
    solve(A.sum)
  }
}
