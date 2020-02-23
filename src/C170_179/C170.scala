object C170 {

  object P1 {
    def f(l:List[Char], acc:List[Char]):String = l match {
      case h1::h2::'#'::t => f(t, ((h1 - '0')*10 + (h2-'0') - 1 + 'a').toChar::acc)
      case Nil => acc.reverse.mkString
      case h::t => f(t, ((h - '0' - 1) + 'a').toChar::acc)
    }
    def freqAlphabets(s: String): String = f(s.toList, Nil)
  }

  // Memory
  object P2 {
    def xorQueries(A: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
      A.indices.tail foreach {i => A(i) ^= A(i-1)}
      queries map {
        case Array(0, r) => A(r)
        case Array(l, r) => A(r) ^ A(l-1)
      }
    }
  }

  // not pass case 42
  object P3 {
    def watchedVideosByFriends(watchedVideos: List[List[String]], friends: Array[Array[Int]], id: Int, level: Int): List[String] = {
      val vid = watchedVideos.toArray
      def f(l:List[Int],step:Int, seen:Set[Int] = Set.empty[Int]):List[Int] = {
        if(step == 0) l else f(l.flatMap{ i => friends(i) filterNot {
          (l ++ seen).contains
        }}, step-1, seen ++ l)
      }
      f(List(id), level)
        .flatMap {i => vid(i)}
        .groupBy{x => x}
        .toList
        .sortBy{case (k,v) => (v.length, k)}
        .map{case (k,v) => k}
    }
  }

  /* p3, python 3 code :
  class Solution:
    def watchedVideosByFriends(self, watchedVideos: List[List[str]], friends: List[List[int]], id: int, level: int) -> List[str]:
        C, R = set(), {id}
        for i in range(level):
            C |= R
            R = set(sum([friends[r] for r in R], [])) - C
        D = collections.Counter(sum([watchedVideos[r] for r in R], []))
        return sorted(D, key = lambda a: (D[a], a))
   */
}
