package C150_159

/**
  * Created by lilisun on 10/27/19.
  */
object C159 {
  object Solution {
    case class Fre(q:Int, w:Int, e:Int, r:Int){
      def +(ch:Char):Fre = ch match {
        case 'Q' => Fre(q+1, w, e,r)
        case 'W' => Fre(q, w+1, e, r)
        case 'E' => Fre(q, w, e+1, r)
        case 'R' => Fre(q, w, e , r+1)
      }
      def -(that:Fre) = Fre(q - that.q, w - that.w,e-that.e, r - that.r)
      def length():Int = q + w + e + r
      def non_neg():Fre = Fre( q max 0, w max 0 , e max 0, r max 0)
      def > (that:Fre):Boolean = {
        q >= that.q &&
        w >= that.w &&
        e >= that.e &&
        r >= that.r
      }
    }


    def slide_window(base:Fre, str:String)(l:Int, r:Int, dir:Boolean, ans:Int, acc:Fre):Int = dir match {
      case true =>
        if(r+1 == str.length) slide_window(base, str)(l, r, false, ans, acc)
        else {
          if (acc + str(r) > base)
        }
      case false =>

    }

    def balancedString(s: String): Int = {
      val avg = s.length / 4
      var fre = Fre(0,0,0,0)
      s foreach {ch => fre = fre + ch}
      fre = (fre - Fre(avg, avg, avg, avg)).non_neg()
      slide_window(fre, s)(0,0, true,  s.length, Fre(0,0,0,0))
    }
  }

}
