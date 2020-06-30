object C140 {
  object P1 {
    def findOcurrences(text: String, a: String, b: String): Array[String] = {
      val words = text.split(' ').toList
      def f(l:List[String], acc:List[String]):List[String] = l match {
        case `a`::`b`::h::t => f(l.tail, h::acc)
        case h::t => f(t, acc)
        case Nil => acc
      }
      f(words, Nil).reverse.toArray
    }
  }
  object P2 {
    def numTilePossibilities(tiles: String): Int = {
      val A=Array.fill(26)(0)
      tiles foreach {ch => A(ch - 'A')+=1}
      def backtrace(A:Array[Int]):Int = {
        var ret = 0
        for{i <- 0 until 26 if A(i) > 0}{
          ret += 1
          A(i)-=1
          ret += backtrace(A)
          A(i)+=1
        }
        ret
      }
      backtrace(A)
    }
  }
  object P3 {
    def sufficientSubset(root: TreeNode, limit: Int): TreeNode = {
      def dfs(root:TreeNode,par:TreeNode, cum:Int):Boolean = {
        if(root == null) return true
        if(root.left == null && root.right == null) return cum+root.value < limit
        val l = dfs(root.left, root, cum + root.value)
        val r = dfs(root.right, root, cum + root.value)
        if(l) root.left = null
        if(r) root.right = null
        l && r
      }
      val guard = new TreeNode(0)
      guard.left = root
      dfs(guard, null, 0)
      guard.left
    }
  }
  object P4 {
    def smallestSubsequence(text: String): String = {
      val cnt = Array.fill(26)(0)
      text foreach {ch => cnt(ch-'a') += 1}
      def f(l:List[Char], acc:List[Char]):List[Char] = {
        // println(l, acc)
        l match {
          case Nil => acc
          case h::t =>
            if(acc.isEmpty) {
              cnt(h - 'a') -= 1
              f(t, h::acc)
            }
            else {
              if(acc.contains(h)) {
                cnt(h - 'a') -= 1
                f(t, acc)
              }

              else if (h < acc.head && cnt(acc.head - 'a') >= 1) f(l, acc.tail)
              else {
                cnt(h- 'a') -= 1
                f(t, h::acc)
              }
            }
        }
      }
      f(text.toList, Nil).reverse.mkString
    }
  }
}
