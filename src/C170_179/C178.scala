object C178 {
  def smallerNumbersThanCurrent(nums: Array[Int]): Array[Int] = {
    nums.indices.toArray.map{i => nums.zipWithIndex.count{case (x,j) => x < nums(i) && i!=j}}
  }
  def f(A:Array[Int]):BigInt = {
    var ans = BigInt(0)
    for{i <- A.indices} ans = ans * 37 + A(i)
    ans
  }
  def rankTeams(votes: Array[String]): String = {
    val A = Array.fill(26,26)(0) // A-Z
    votes foreach {str =>
      str.zipWithIndex foreach {
        case (ch, rank) => A(ch - 'A')(rank) += 1
      }
    }
    A.zipWithIndex
      .filter{case (arr, i) => arr.exists{_ > 0}}
      .sortBy{case (arr, i) => -f(arr)}
      .map{case (arr, i) => ((i + 'A').toChar).toString}
      .mkString
  }
  def f(head: ListNode, root: TreeNode): Boolean = {
    if(head == null) true
    else if(root == null) false
    else {
      if(root.value == head.x)
        f(head.next, root.left) || f(head.next, root.right)
      else false
    }
  }
  def isSubPath(head: ListNode, root: TreeNode): Boolean = {
    if(head == null) true
    else if(root == null) false
    else {
      if(root.value == head.x && f(head, root)) true
      else isSubPath(head, root.left) || isSubPath(head, root.right)
    }
  }
}
