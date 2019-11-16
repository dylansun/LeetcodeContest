package B10_19

/**
  * Created by lilisun on 11/16/19.
  */
object B13 {
  object p1 {
    // 0 => ""
    // 0~1  , 2 => "0", "1"
    // 2~5  , 2^2 => "00", "01", "10", "11",
    //
    // 6~13 , 2^3 => "000",
    // g(i) => {"0"+g(i-1)}, {"1"+g(i-1)}
    // x => i?
    // g(0), g(1), ... g(i)
    // where is n in the seq?
    // n = 3
    // 3 > 2, 3不在第1个序列，
    // (3-2) < 4, 3在第2个序列，
    // 3在序列的左半部分 还是右半部分？
    // 1 < 4 / 2, 在左半部分， 相当于1在第一个序列额第一个值
    def f(n:Int, i:Int, acc:String = ""):String = (n, i) match {
      case (1,1) => acc + "0"
      case (2,1) => acc + "1"
      case (_,_) =>
        if (n > Math.pow(2, i)) f(n - Math.pow(2, i).toInt, i + 1, acc)
        else {
          if (n > Math.pow(2, i - 1)) f(n - Math.pow(2, i - 1).toInt, i - 1, acc + "1")
          else f(n, i - 1, acc + "0")
        }
      }
    def encode(n: Int): String = n match {
      case 0 => ""
      case _ => f(n, 1)
    }
  }

  object P2 {
    type LS = List[String]
    def findSmallestRegion(regions: List[List[String]], region1: String, region2: String): String = {
      val parent = scala.collection.mutable.HashMap[String, String]()
      regions foreach {case h::t => t foreach {place => parent.put(place, h)}}
      def f(region:String, acc:List[String] = Nil):List[String] = {
        if(parent.contains(region)) f(parent(region), region::acc)
        else region::acc
      }

      def getMin(l1:LS, l2:LS):String = (l1, l2) match {
        case (h1::h2::t1, h3::h4::t2) if h1 == h3 && h2 == h4 => getMin(h2::t1, h4::t2)
        case (h1::t1, h2::t2) if h1 == h2 => h1
        case (_,_) => ""
      }

      getMin(f(region1), f(region2))
    }
  }

  object P3 {
    class DSU (N:Int){
      val parent = (0 until N).toArray
      def union(x:Int, y:Int):Unit = if(parent(x) != parent(y)) parent(x) = parent(y)
      def find(x:Int):Int = if(x!= parent(x)) find(parent(x)) else x
    }
    type LLS = List[List[String]]
    type LS = List[String]
    // Step 1: merge synonyms
    // (a, b), (b, c) => (a, b, c)

    def generateSentences(synonyms: LLS, text: String): LS = {
      val vocabs = synonyms.flatten.distinct
      val id = scala.collection.mutable.HashMap[String, Int]()
      def f(l:LS,i:Int):Unit = l match {
        case Nil => {}
        case h::t => id.put(h, i); f(t, i+1)
      }
      f(vocabs,0)
      val dsu = new DSU(vocabs.length)
      synonyms foreach {case h1::h2::Nil => dsu.union(id(h1), id(h2))}

      val table = scala.collection.mutable.HashMap[Int, LS]()
      vocabs foreach {str =>
        val pid = dsu.find(id(str))
        table.put(pid, (str::table.getOrElse(pid, Nil)).sorted)
      }
      def solve(ls:LS, lls:LLS = Nil):LS = ls match {
        case Nil => lls map {l => l.reverse.mkString(" ")}
        case h::t =>
          if(vocabs.contains(h))
            solve(t, lls flatMap {l => table(dsu.find(id(h))) map {x => x::l}})
          else
            solve(t, lls map {l => h::l})
      }

      text.split(" ").toList match {
        case Nil => Nil
        case h::t =>
          if(vocabs.contains(h))
            solve(t, table(dsu.find(id(h))).map(x => x::Nil))
          else
            solve(t, List(h::Nil))
      }
    }
  }

  object P4 {
    // 2 => 1
    // 4 => 2
    // 6 => 1 + 2 + 2
    // 8 => f(6) + f(2)*f(4) + f(4)* f(2) + f(6)*f(0) = 5 + 5 + 2 +2
    val mod = (1e9+7).toInt
    def numberOfWays(n: Int): Int = {
      val dp = Array.fill(1001)(BigInt(0))
      dp(0) = 1
      for{i <- 2 to 1000}
        dp(i) = (for{j <- 0 to i-2 by 2} yield dp(j)*dp(i-2-j)).reduce((x,y) => (x + y) % mod)

      dp(n).toInt
    }
  }

  def main(args: Array[String]): Unit = {
    println(p1.encode(107))
  }
}
