class WordFilter(_words: Array[String]) {
  val pstrie = new PSTrie()
  _words.zipWithIndex foreach {case (word, i) =>
    pstrie.insert(word.toList, word.toList.reverse, i)
  }
  def f(pre: String, suf: String): Int = {
    println(pre, suf, pre.zipAll(suf.reverse, ' ',' ').toList)
    pstrie.search(pre.zipAll(suf.reverse, ' ',' ').toList)
  }

}
