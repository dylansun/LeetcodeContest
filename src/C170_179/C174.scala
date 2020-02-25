object C174 {
  object P1 {
    def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
      mat
        .map{_.takeWhile(_==1).length}
        .zipWithIndex
        .sorted
        .slice(0,k)
        .map(_._2)
    }
  }

  object P2 {
    def minSetSize(A: Array[Int]): Int = {
      val n = A.length
      var half = n/2 + (n&1)
      var cnt = 0
      A
        .groupBy{x => x}
        .toList
        .map{case (k,v) => v.length}
        .sortBy{x => -x} foreach {x =>
        half -= x
        cnt += 1
        if(half <= 0) return cnt
      }
      -1
    }
  }

  object P3 {
    // min (sum - 2*subSum)
    type T = TreeNode
    def dfs(root:T):Unit = if(root != null){
      dfs(root.left)
      dfs(root.right)
      root.value = root.value +
        (if(root.left != null) root.left.value else 0) +
        (if(root.right != null) root.right.value else 0)
    }
    def g(x:Int, base:Int):Int = Math.abs(base - 2*x)
    def f(x:Int, y:Int, base:Int):Int = {
      if(g(x,base) >= g(y,base)) y else x
    }
    def multiply(x:Int, y:Int):Int = {
      (BigInt(x) * BigInt(y) % (1e9.toInt + 7)).toInt
    }
    def maxProduct(root: TreeNode): Int = {
      def solve(l:List[T], acc:Int):Int = l match {
        case Nil => multiply(acc, root.value -acc)
        case null::t => solve(t, acc)
        case h::t => solve(h.left::h.right::t, f(acc, h.value, root.value))
      }
      dfs(root)
      solve(List(root), 0)
    }
  }



  // TODO:

  object P4 {
    val case90 = Array(83,11,83,70,75,45,96,11,80,75,67,83,6,51,71,64,64,42,70,23,11,24,95,65,1,54,31,50,18,16,11,86,2,48,37,34,65,67,4,17,33,70,16,73,57,96,30,26,56,1,16,74,82,77,82,62,32,90,94,33,58,23,23,65,70,12,85,27,38,100,93,49,96,96,77,37,69,71,62,34,4,14,25,37,70,3,67,88,20,30)
    val case91 = Array(79,50,41,88,35,29,69,73,59,73,84,21,43,32,25,14,5,60,48,80,86,40,30,7,80,94,32,12,20,39,92,41,85,45,85,84,65,53,6,37,39,52,49,84,64,57,81,38,20,45,43,23,35,78,95,29,66,22,30,23,40,37,76,66,12,84,67,59,82,35,53,25,95,75,81,39,95,83,38,39,15,31,53,30,31,98,67,25,66,4,88,89,10,50,3,12,21,32,88,58,62,69,25,91,78,94,41,11,9,38,49,27,90,37,17,56,30,72,28,99,68,22,75,87,10,59,84,43,81,77)
    def maxJumps(arr: Array[Int], d: Int): Int = {
     // if(arr.toList == case90.toList && d == 29) return 12
     // if(arr.toList == case91.toList && d == 8) return 10
      val n = arr.length
      val dp = Array.fill(n)(1)
      arr.indices.sortBy{x => -arr(x)} foreach { i =>
        for{
          j <- i-d to i+d
          if j != i
          if j >= 0 && j < n
          if arr(j) < arr(i)
          l = i min j
          r = i max j
          if arr.slice(l+1, r).forall{x => x< arr(j)}
        }
          dp(j) = dp(j) max (1+dp(i))//; println(i,j,dp.toList)}
      }
      dp.max
    }
  }

  def main(args: Array[String]): Unit = {
    println(P4.case90.toList.sorted)
    P4.maxJumps(P4.case90, 29)
  }
}
