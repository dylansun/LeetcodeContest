object C191 {
  object P1 {
    def maxProduct(nums: Array[Int]): Int = {
      (for{
        i <- nums.indices
        j <- i+1 until nums.length
      } yield (nums(i)-1) * (nums(j)-1)).max
    }
  }
  object P2 {
    def maxArea(h: Int, w: Int, horizontalCuts: Array[Int], verticalCuts: Array[Int]): Int = {
      def f(A:Array[Int], b:Int):Int = {
        (for{i <- A.indices}yield{
          if(i==0) A.head
          else A(i) - A(i-1)
        }).max max b - A.last
      }
      SafeCal.mul(f(horizontalCuts.sorted, h),f(verticalCuts.sorted, w))
    }
  }
  object P3 {
    def minReorder(n: Int, connections: Array[Array[Int]]): Int = {
      var vis = Set(0)
      var l = List(0)
      var ret = 0
      val G_t = new Graph[Int]()
      val G_f = new Graph[Int]()
      connections foreach {case Array(f, t) => G_t.add(f,t); G_f.add(t, f)}
      while(vis.size < n){
        val h = l.head
        l = l.tail
        vis = vis + h
        for{x <- G_f.get(h) if !vis.contains(x)} {l::=x}
        for{x <- G_t.get(h) if !vis.contains(x)} {l::=x;ret+=1}
      }
      ret
    }
  }
  object P4 {
    def getProbability(balls: Array[Int]): Double = {
      val frac = Array.fill(49)(1.0)
      for{i <- 2 to 48} frac(i) = i * frac(i-1)
      def f(l:List[Int]): Double = {
        val n = l.sum
        var ret = frac(n)
        for{x <-l} ret /= frac(x)
        ret
      }
      val total = f(balls.toList)
      val n = balls.sum >> 1;
      var cnt = 0.0
      def solve(i:Int, n1:Int, n2:Int, l1:List[Int], l2:List[Int], cnt1:Int, cnt2:Int):Unit = {
        if(cnt1 > n || cnt2 > n) return
        if(i==balls.length) {if(n1==n2) cnt += f(l1)*f(l2);return}
        for{x <- 0 to balls(i)}{
          if(x == 0)
            solve(i+1,n1,n2+1,l1,balls(i)::l2, cnt1, cnt2+balls(i))
          else if(x == balls(i))
            solve(i+1,n1+1,n2, balls(i)::l1, l2, cnt1+balls(i), cnt2)
          else
            solve(i+1,n1+1,n2+1,x::l1,(balls(i)-x)::l2, cnt1+x, cnt2+balls(i)-x)

        }
      }
      solve(0,0,0,Nil, Nil, 0,0)
      cnt / total
    }
  }
  def main(args: Array[String]): Unit = {
    println(P4.getProbability(Array(6,6,6,6,6,6)))
  }
}
