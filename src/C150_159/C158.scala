package C150_159

/**
  * Created by lilisun on 10/13/19.
  */
object C158 {
  object P1 {
    def balancedStringSplit(s: String): Int = {
      def f(l:List[Char], cnt:Int, acc:Int):Int = l match {
        case Nil => acc
        case 'L'::t => f(t, cnt+1, acc  + g(cnt + 1))
        case 'R'::t => f(t, cnt-1, acc  + g(cnt - 1))
      }
      def g(x:Int):Int = if(x == 0) 1 else 0

      f(s.toList,0,0)
    }
  }
  object P2 {
    case class Pos(x:Int, y:Int){
      def +(that:Pos):Pos = Pos(x+that.x, y + that.y)
    }
    val dir = Array(Pos(-1,-1), Pos(0,-1), Pos(0,1), Pos(1,0), Pos(-1,0), Pos(1,1), Pos(-1,1), Pos(1,-1))
    def queensAttacktheKing(queens: Array[Array[Int]], king: Array[Int]): List[List[Int]] = {
      def valid(pos:Pos):Boolean = {
        pos.x >= 0 &&
          pos.y >= 0 &&
          pos.x < 8  &&
          pos.y < 8
      }
      val ans = Array.fill(8)(false)
      val start = Array.fill(8)(Pos(king(0),king(1)))
      var A = queens.map{case Array(x,y) => Pos(x,y)}
      var l = List.empty[List[Int]]
      while(start exists valid){
        for{i <- 0 until 8}{
          start(i) = start(i) + dir(i)
          if(A.contains(start(i)) && !ans(i)) {
            ans(i) = true
            l ::= List(start(i).x, start(i).y)
          }
        }
      }
      l
    }
  }
  object P3 {
    /*
        TODO: solve this problem in dp
     */

    /*
        const int MOD = 1e9 + 7;
        const int N = 5e3 + 10;
        int dp[N][6][20];

        class Solution {
        public:
            int n;
            vector<int> a;
            int solve(int pos, int last, int cnt) {
                if (pos == n) return 1;
                int& ret = dp[pos][last][cnt];
                if (ret >= 0) return ret;
                ret = 0;
                for (int i = 0; i < 6; ++i) {
                    if (last == i && cnt + 1 > a[i]) continue;
                    ret = (ret + solve(pos + 1, i, last == i ? cnt + 1 : 1)) % MOD;
                }
                return ret;
            }
            int dieSimulator(int n, vector<int>& rollMax) {
                memset(dp, 255, sizeof(dp));
                this->a = rollMax;
                this->n = n;
                int ret = 0;
                for (int i = 0; i < 6; ++i) {
                    ret = (ret + solve(1, i, 1)) % MOD;
                }
                return ret;
            }
        };
     */

  }
  object P4 {
    case class Elem(n:Int, times:Int)
    def maxEqualFreq(nums: Array[Int]): Int = {
      val fre = Array.fill(100001)(0)
      val n = nums.length
      nums foreach {i => fre(i) += 1}

      def p(A:Array[Int]):Boolean = {
        val l = fre.filter{x => x != 0}.groupBy{x=>x}.toList.map{case (k,v) => Elem(k,v.length)}.sortBy{case Elem(n, times) => n}
        println(l)
        l match {
          case Elem(_,1)::Nil => true
          case Elem(1, _)::Nil =>true
          case Elem(1,1)::Elem(_,_)::Nil => true
          case Elem(x,_)::Elem(y,1)::Nil => y-x == 1
          case _ => false
        }
      }
      for{i <- n to 1 by -1}{
        if(i!=n) fre(nums(i)) -= 1
        if(p(fre)) return i
      }
      1
    }
  }

}
