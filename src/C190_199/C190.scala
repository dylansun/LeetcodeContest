import language.postfixOps
object C190 {
  object P1 {
    def isPrefixOfWord(sentence: String, searchWord: String): Int = {
      sentence.split(" ").zipWithIndex foreach {case (str, i) => if(str.startsWith(searchWord)) return i+1}
      -1
    }


  }
  object P2 {
    val v = "aeiou"
    def maxVowels(s: String, k: Int): Int = {
      var ret = s.slice(0,k).map(x => f(x, v)).sum
      var tmp = ret
      (k until s.length) foreach  {i =>
        tmp = tmp + f(s(i), v) - f(s(i-k), v)
        ret = tmp max ret
      }
      ret
    }
    def f(ch:Char, str:String):Int = if(str.contains(ch)) 1 else 0
  }
  object  P2_functional {
    val v = "aeiou"
    def maxVowels(s: String, k: Int): Int = {
      def delta(i:Int):Int = f(s(i)) - f(s(i-k))
      def f(ch:Char):Int = if(v.contains(ch)) 1 else 0
      def solve(i:Int, cur:Int, ret:Int = 0):Int =
        if(i >= s.length) ret max cur  else solve(i+1, cur + delta(i), ret max cur )

      solve(k, s.slice(0,k) map f sum)
    }
  }
  object P3 {
    def pseudoPalindromicPaths (root: TreeNode): Int = {
      if(root == null) return 0
      var ret = 0
      def dfs(root:TreeNode, set:Set[Int]):Unit = {
        if(root.left == null && root.right == null){
          if(f(root.value, set).size <= 1) ret += 1
        } else {
          if(root.left != null) dfs(root.left, f(root.value, set))
          if(root.right != null) dfs(root.right, f(root.value, set))
        }
      }
      dfs(root, Set.empty[Int])
      ret
    }

    def f(x:Int, set:Set[Int]):Set[Int] = if(set.contains(x)) set - x else set + x

  }
  object P3_2 {
    def pseudoPalindromicPaths (root: TreeNode): Int = {
      var ret = 0
      def dfs(root:TreeNode, k:Int):Unit = {
        if(isLeaf(root)) {if(k==0 || (k & -k) == k) ret += 1} else
          List(root.left, root.right) foreach { child =>
            if(child != null) dfs(child, f(k, child.value))
          }
      }
      if(root == null) 0 else {dfs(root, 1 << root.value); ret}
    }
    def f(k:Int, bit:Int):Int = k ^ (1 << bit)
    def isLeaf(root:TreeNode):Boolean =
      root.left == null && root.right == null
  }

  // use for-expression
  object P3_3 {
    def pseudoPalindromicPaths (root: TreeNode): Int = {
      var ret = 0
      def incRet(k:Int):Unit = if(k==0 || (k & -k) == k) ret += 1
      def dfs(root:TreeNode, k:Int):Unit = {
        if(isLeaf(root)) incRet(k) else
          for {
            child <- List(root.left, root.right)
            if child != null
          } dfs(child, k ^ (1 << child.value))
      }
      if(root == null) 0 else {dfs(root, 1 << root.value); ret}
    }
    def f(k:Int, bit:Int):Int = k ^ (1 << bit)
    def isLeaf(root:TreeNode):Boolean =
      root.left == null && root.right == null
  }
  object P3_4 {
    def pseudoPalindromicPaths (root: TreeNode): Int = {
      def isPseudo(k:Int):Int = if(k==0 || (k & -k) == k) 1 else 0
      def dfs(root:TreeNode, k:Int):Int =
        if(root == null) 0 else {
        if(isLeaf(root)) isPseudo(k) else
          (for {
            child <- List(root.left, root.right)
            if child != null
          } yield dfs(child, k ^ (1 << child.value))).sum
      }
      if(root == null) 0 else dfs(root, 1 << root.value)
    }
    def f(k:Int, bit:Int):Int = k ^ (1 << bit)
    def isLeaf(root:TreeNode):Boolean =
      root.left == null && root.right == null
  }
  object P4 {
    val init = -100000
    // dp(i)(j) = max(dp(i-1)(k) + a(i)(k+1)...) max (dp(k-1)(j) + a(k)b(j))
    def maxDotProduct(a: Array[Int], b: Array[Int]): Int = {
      val dp = Array.fill(a.length, b.length)(init)
      // i = 0 or j = 0
      for{i <- a.indices} dp(i)(0) = 0 max (if(i==0) a(i)*b(0) else dp(i-1)(0) max a(i)*b(0))
      for{j <- b.indices} dp(0)(j) = 0 max (if(j==0) a(0)*b(j) else dp(0)(j-1) max a(0)*b(j))
      for{
        i <- 1 until a.length
        j <- 1 until b.length
      }{
        dp(i)(j) = dp(i)(j) max dp(i-1)(j) max dp(i)(j-1) max (dp(i-1)(j-1) + a(i)*b(j))
        for{ii <- 0 until i}
          dp(i)(j) = dp(i)(j) max (if(ii==0) a(0)*b(j) else a(ii)*b(j)+dp(ii-1)(j-1))
        for{jj <- 0 until j}
          dp(i)(j) = dp(i)(j) max (if(jj==0) a(i)*b(0) else a(i)*b(jj)+dp(i-1)(jj-1))
      }
      if(dp.last.last == 0) a.max * b.max max a.min*b.max max a.max*b.min max a.min *b.min
      else dp.last.last
    }
  }
  object P4_2 {
    val init = -100000
    def maxDotProduct(a: Array[Int], b: Array[Int]): Int = {
      val dp = Array.fill(a.length, b.length)(init)
      def f(i:Int, j:Int):Int = if(i>=0 && j >= 0) dp(i)(j) else 0
      for{ i <- a.indices;j <- b.indices}
        dp(i)(j) = dp(i)(j) max f(i-1,j) max f(i,j-1) max (f(i-1,j-1) + a(i)*b(j))
      if(dp.last.last == 0) solve(a,b) else dp.last.last
    }
    def solve(a: Array[Int], b: Array[Int]):Int = - minAbs(a)*minAbs(b)
    def minAbs(a:Array[Int]):Int = a map math.abs min
  }
  def test4(): Unit ={
    //[-3,-8,3,-10,1,3,9]
    //[9,2,3,7,-9,1,-8,5,-1,-1]
    val a = Array(-3,-1)
    val b = Array(9,1)
    println(P4.maxDotProduct(a,b))
  }
  def main(args: Array[String]): Unit = {
    test4()
    for{i <- 1 to 100}{
      println(i, (i & -i) == i)
    }
  }

}
