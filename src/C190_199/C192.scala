import scala.collection.mutable.{ArrayBuffer => MAB}
object C192 {
  class BrowserHistory(_homepage: String) {
    var hist = MAB[String](_homepage)
    var i = 0
    def visit(url: String) {
      hist = hist.slice(0,i+1)
      hist.append(url)
      i += 1
    }

    def back(steps: Int): String = {
      i = 0 max i - steps
      hist(i)
    }

    def forward(steps: Int): String = {
      i = hist.length -1 min  i + steps
      hist(i)
    }

  }
  object P4 {
    val inf = 1e9.toInt
    def post_ans(x:Int):Int = if(x >= inf) -1 else x
    def minCost(houses: Array[Int], cost: Array[Array[Int]], m: Int, n: Int, target: Int): Int = {
      val dp=Array.fill(m,n,target+1)(inf)
      // initial, target = 1
      // i = 0
      if(houses(0) == 0){
        for{j <- 0 until n} dp(0)(j)(1) = cost(0)(j)
      } else dp(0)(houses(0)-1)(1) = 0

      // t = 1 to target
      for{
        t <- 1 to target
        i <- 1 until m
        j <- 0 until n
      }{

        if(houses(i) == 0){
          dp(i)(j)(t) = (dp(i-1)(j)(t)+cost(i)(j)) min dp(i)(j)(t)
          for{jj <- 0 until n if jj != j}
            dp(i)(j)(t) = dp(i)(j)(t) min (dp(i-1)(jj)(t-1) + cost(i)(j))
        }
        else if(houses(i)-1 == j){
          dp(i)(j)(t) =dp(i)(j)(t) min dp(i-1)(j)(t)
          for{jj <- 0 until n if jj != j}
            dp(i)(j)(t) = dp(i)(j)(t) min dp(i-1)(jj)(t-1)
        }
      }
      post_ans((for{j <- 0 until n} yield dp(m-1)(j)(target)).min)
    }
  }

  def test4(){
    val houses = Array(0,0,0,0,0)
    val m = 5; val n = 2; val target = 3;
    val cost = Array(
      Array(1,10),
      Array(10,1),
      Array(10,1),
      Array(1,10),
      Array(5,1)
    )
    println(P4.minCost(houses, cost,m,n,target))
  }
  def main(args: Array[String]): Unit = {
    test4()
  }
}
