package C140_149

/**
  * Created by lilisun on 6/16/19.
  */
import scala.collection.mutable
object C141 {
  def duplicateZeros(A: Array[Int]): Unit = {
    def f(l:List[Int], acc:List[Int]):List[Int] = l match {
      case Nil => acc.reverse
      case 0::t => f(t, 0::0::acc)
      case h::t => f(t, h::acc)
    }

    val B = f(A.toList, Nil).toArray
    A.indices foreach {i => A(i) = B(i)}

  }

  case class Point(x:Int,y:Int){
    def +(that:Point):Point = Point(x + that.x, y + that.y)
  }
  val dir = List(Point(0,1),Point(0,-1),
    Point(1,-1),Point(1,0), Point(1,1),
    Point(-1, 0), Point(-1,1), Point(-1,-1))
  def shortestPathBinaryMatrix(grid: Array[Array[Int]]): Int = {
    val start = Point(0,0)
    val n = grid.length
    val visited = mutable.Set(start)
    val end = Point(n-1,n-1)
    def traverse(l:List[Point], acc:Int):Int = {
      if(l.isEmpty) -1
      else if(l.contains(end)) acc
      else{
        val nl = (l flatMap f).distinct filter notBlock filterNot visited.contains
        nl.foreach {x => visited += x}
        traverse(nl, acc + 1)
      }
    }
    def inBound(p:Point):Boolean = {
      p.x >= 0 && p.y >= 0 && p.x < n && p.y < n
    }
    def notBlock(p:Point):Boolean = grid(p.x)(p.y) == 0
    def f(p:Point):List[Point] = {
      dir.map(_+p) filter inBound
    }

    traverse(List(start), 1)

  }

  def largestValsFromLabels(values: Array[Int], labels: Array[Int], num_wanted: Int, use_limit: Int): Int = {
    (values zip labels)
      .groupBy{case (x,y) => y}.values.toList.map(_.toList)
      .flatMap{x =>
        if(x.length > use_limit)
          x.sortBy{case (a,b) => -a}.slice(0, use_limit)
        else x
      }
      .sortBy{case (x,y) => -x}.slice(0, num_wanted).map(_._1).sum
  }

  def analysisDP(l:List[(Int, Int, Int)], acc:List[(Int, Int, Int)]): List[(Int, Int)] = l match {
    case Nil => acc.map{case (dp, i,j) => (i,j)}
    case _ =>
      // select the maxvalue of dp
      val max_dp = l.map(_._1).max
      val select = l
        .filter{case (dp, i ,j) => dp == max_dp}
        .sortBy{case (dp, i,j) => i + j}
        .head
      analysisDP(l.filter{case (dp, i,j) => i < select._2 && j < select._3}, select::acc)
  }
  def generateAns(str1:String, str2:String, A:Array[(Int, Int)])(i:Int, acc:String):String = {
    if(i == 0){
      generateAns(str1, str2, A)(i + 1, acc + str1.slice(0, A(0)._1) + str2.slice(0, A(0)._2) + str1(A(0)._1))
    }
    else if (i >0 && i < A.length - 1){
      generateAns(str1, str2, A)(i + 1, acc + str1.slice(A(i-1)._1 + 1, A(i)._1) + str2.slice(A(i-1)._2 + 1, A(0)._2) + str1(A(i)._1))
    }
    else{
      acc + str1.slice(A(i-1)._1 + 1, A(i)._1) +
        str2.slice(A(i-1)._2 + 1, A(0)._2) +
        str1(A(i)._1) +
        str1.slice(A(i)._1 + 1, str1.length) +
        str2.slice(A(i)._2 + 1, str2.length)
    }
  }
  def shortestCommonSupersequence(str1: String, str2: String): String = {
    val dp = Array.fill(str1.length + 1, str2.length + 1)(0)
    var l = List.empty[(Int,Int,Int)]
    for{
      i <- 0 until str1.length
      j <- 0 until str2.length
    }{
      if(str1(i) == str2(j)){
        dp(i+1)(j+1) = 1 + dp(i)(j)
        l ::= (dp(i+1)(j+1), i, j)
      }
      else
        dp(i+1)(j+1) = dp(i)(j+1) max dp(i+1)(j)
    }
    val pos = analysisDP(l, Nil)

    var ans = List.empty[Char]
    var i = 0
    var j = 0
    var p = pos
    while(p.nonEmpty){
      while(i < str1.length && i < p.head._1) {
        ans ::= str1(i)
        i+= 1
      }
      while(j < str2.length && str2(j) != str1(p.head._1)){
        ans ::=str2(j)
        j+=1
      }
      ans ::= str1(p.head._1)
      p = p.tail
      i += 1
      j += 1
    }


    val Ans2 = ans.reverse.mkString +
      (if(i < str1.length) str1.substring(i) else "") +
      (if(j < str2.length) str2.substring(j) else "")

    val Ans = generateAns(str1, str2, pos.toArray)(0, "")
    println(Ans2 == Ans)
    Ans2

  }

  def main(args: Array[String]): Unit = {

    val a = "atdznrqfwlfbcqkezrltzyeqvqemikzgghxkzenhtapwrmrovwtpzzsyiwongllqmvptwammerobtgmkpowndejvbuwbporfyroknrjoekdgqqlgzxiisweeegxajqlradgcciavbpgqjzwtdetmtallzyukdztoxysggrqkliixnagwzmassthjecvfzmyonglocmvjnxkcwqqvgrzpsswnigjthtkuawirecfuzrbifgwolpnhcapzxwmfhvpfmqapdxgmddsdlhteugqoyepbztspgojbrmpjmwmhnldunskpvwprzrudbmtwdvgyghgprqcdgqjjbyfsujnnssfqvjhnvcotynidziswpzhkdszbblustoxwtlhkowpatbypvkmajumsxqqunlxxvfezayrolwezfzfyzmmneepwshpemynwzyunsxgjflnqmfghsvwpknqhclhrlmnrljwabwpxomwhuhffpfinhnairblcayygghzqmotwrywqayvvgohmujneqlzurxcpnwdipldofyvfdurbsoxdurlofkqnrjomszjimrxbqzyazakkizojwkuzcacnbdifesoiesmkbyffcxhqgqyhwyubtsrqarqagogrnaxuzyggknksrfdrmnoxrctntngdxxechxrsbyhtlbmzgmcqopyixdomhnmvnsafphpkdgndcscbwyhueytaeodlhlzczmpqqmnilliydwtxtpedbncvsqauopbvygqdtcwehffagxmyoalogetacehnbfxlqhklvxfzmrjqofaesvuzfczeuqegwpcmahhpzodsmpvrvkzxxtsdsxwixiraphjlqawxinlwfspdlscdswtgjpoiixbvmpzilxrnpdvigpccnngxmlzoentslzyjjpkxemyiemoluhqifyonbnizcjrlmuylezdkkztcphlmwhnkdguhelqzjgvjtrzofmtpuhifoqnokonhqtzxmimp"
    val b = "xjtuwbmvsdeogmnzorndhmjoqnqjnhmfueifqwleggctttilmfokpgotfykyzdhfafiervrsyuiseumzmymtvsdsowmovagekhevyqhifwevpepgmyhnagjtsciaecswebcuvxoavfgejqrxuvnhvkmolclecqsnsrjmxyokbkesaugbydfsupuqanetgunlqmundxvduqmzidatemaqmzzzfjpgmhyoktbdgpgbmjkhmfjtsxjqbfspedhzrxavhngtnuykpapwluameeqlutkyzyeffmqdsjyklmrxtioawcrvmsthbebdqqrpphncthosljfaeidboyekxezqtzlizqcvvxehrcskstshupglzgmbretpyehtavxegmbtznhpbczdjlzibnouxlxkeiedzoohoxhnhzqqaxdwetyudhyqvdhrggrszqeqkqqnunxqyyagyoptfkolieayokryidtctemtesuhbzczzvhlbbhnufjjocporuzuevofbuevuxhgexmckifntngaohfwqdakyobcooubdvypxjjxeugzdmapyamuwqtnqspsznyszhwqdqjxsmhdlkwkvlkdbjngvdmhvbllqqlcemkqxxdlldcfthjdqkyjrrjqqqpnmmelrwhtyugieuppqqtwychtpjmloxsckhzyitomjzypisxzztdwxhddvtvpleqdwamfnhhkszsfgfcdvakyqmmusdvihobdktesudmgmuaoovskvcapucntotdqxkrovzrtrrfvoczkfexwxujizcfiqflpbuuoyfuoovypstrtrxjuuecpjimbutnvqtiqvesaxrvzyxcwslttrgknbdcvvtkfqfzwudspeposxrfkkeqmdvlpazzjnywxjyaquirqpinaennweuobqvxnomuejansapnsrqivcateqngychblywxtdwntancarldwnloqyywrxrganyehkglbdeyshpodpmdchbcc"
    val ans = shortestCommonSupersequence(a,b)
    println(ans)
  }
}
