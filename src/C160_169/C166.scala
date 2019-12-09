package C160_169

/**
  * Created by lilisun on 12/9/19.
  */
object C166 {
  object P1 {
    def subtractProductAndSum(n: Int): Int = {
      def f(n:Int, acc:List[Int]):List[Int] = {
        if(n > 9) f(n / 10, (n % 10)::acc)
        else n::acc
      }

      val l = f(n, Nil)
      l.product - l.sum
    }
  }

  object P2 {
    // 1 <- ...
    // 2 <- ...
    //
    // [3,3,3,3] => {[0,1,2],}
    type LL = List[List[Int]]
    def groupThePeople(A: Array[Int]): List[List[Int]] = {
      def g(size:Int, l:List[Int], acc:LL = Nil):LL= {
        if(l.size >= size)
          g(size, l.slice(size, l.length), l.slice(0,size) :: acc)
        else if(l.isEmpty)
          acc
        else{
          println(s"Error: $size, $l, $acc")
          Nil
        }
      }
      def f(l:List[(Int,Int)]):List[List[Int]] = g(l.head._1, l.map{case (size, x) => x})
      var ans = List.empty[List[Int]]
      A.zipWithIndex.groupBy{case (size, x) => size} foreach {
        case (size,l) => ans = f(l.toList) ++ ans
      }
      ans
    }
  }

  // binary search: l = 0, r = threshhold
  // f(0,r) => mid = (0 + r) / 2 , if(f(mid), f(0, mid) else f(mid+1, r), if(l == r)
  //
  object P3 {
    def updivise(x:Int, y:Int):Int = if(x % y == 0) x / y else x / y + 1
    def check(nums:Array[Int], div:Int, bound:Int):Boolean = nums.map{x => updivise(x, div)}.sum <= bound
    def binarysearch(A:Array[Int], thresh:Int)(l:Int, r:Int):Int = {
      if(l == r) l
      else if(l == r-1){
        if(check(A, l, thresh)) l else r
      }
      else {
        val mid = ((BigInt(l) + BigInt(r)) >> 1).toInt
        if(check(A,mid, thresh)) binarysearch(A, thresh)(l, mid)
        else binarysearch(A, thresh)(mid+1, r)
      }
    }
    def smallestDivisor(nums: Array[Int], threshold: Int): Int = {
      binarysearch(nums, threshold)(1, Int.MaxValue)
    }
  }

  object P4 {
    // [0,0]
    // [0,1]
    // [a,b]   [a+b+c, a+b+d]
    // [c,d]   [a+c+d, b+c+d]
    // a+b+c % 2 == 0
    // a+b+d % 2 == 0
    // a+c+d % 2 == 0
    // b+c+d % 2 == 1
    // => a +b = 1, c=1,d =1 , a = 0, b = 1
    // [0,1,2]
    // [3,4,5]
    // [6,7,8]

    def checkBit(x:Int, n:Int):Int = {
      val a = 1 << n
      if((x & a) == a) 1 else 0
    }
    def bitsum(x:Int, acc:Int = 0):Int = {
      if(x == 0) acc else bitsum(x >> 1, acc + (x & 1))
    }

    def minFlips(mat: Array[Array[Int]]): Int = {
      val m = mat.length
      val n = mat(0).length
      val nei = List((0,0),(1,0),(-1,0),(0,1),(0,-1))

      def check(x:Int):Boolean = {
        val flipMat = Array.fill(m,n)(0)
        for{
          i <- 0 until m
          j <- 0 until n
        } flipMat(i)(j) = checkBit(x, i*n + j)

        val totalMat = Array.fill(m,n)(0)
        for{
          i <- 0 until m
          j <- 0 until n
        }{
          totalMat(i)(j) = nei.map{case (x,y)=>(x+i,y+j)}.filter{case (x,y) => x>=0 && x < m && y>=0 & y<n}.map{case (x,y) => flipMat(x)(y)}.sum & 1
        }
        totalMat.flatten.toList == mat.flatten.toList
      }
      (0 until 1 << (m*n + 1) ).toList filter check match {
        case Nil => -1
        case h::t => (h::t) map {x => bitsum(x,0)} min
      }
    }
  }
  def test4():Unit = {
    println(P4.minFlips(Array(Array(0,0), Array(0,1))))
  }
  def test3():Unit = {
    println(P3.smallestDivisor(Array(1,2,5,9),6) == 5)
    /*
    输入：nums = [2,3,5,7,11], threshold = 11
    输出：3
     */
    println(P3.smallestDivisor(Array(2,3,5,7,11),11) == 3)
    println(P3.smallestDivisor(Array(19),5)==4)
    /*
    [962551,933661,905225,923035,990560]
    10
    495280
     */
    println(P3.smallestDivisor(Array(962551,933661,905225,923035,990560),10), 495280)

  }
  def test2():Unit = {
    val groupSizes = Array(1,2,2,3,3,3)
    P2.groupThePeople(groupSizes) foreach println
  }
  def main(args: Array[String]): Unit = {
    test4()
  }
}
