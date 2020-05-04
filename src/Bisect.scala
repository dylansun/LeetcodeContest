import scala.collection.mutable.{ArrayBuffer => MAB}
object Bisect {
  type AB = MAB[Int]
  def insort(ab:AB, x:Int):Unit = insort_right(ab, x)
  def insort_right(ab: AB, x: Int): Unit = ab.insert(bisect(ab, x), x)
  def insort_left(ab:AB, x:Int):Unit = ab.insert(bisect_left(ab,x))
  def bisect(ab:AB, x:Int):Int = bisect_right(ab, x)
  def bisect_right(ab: AB, x: Int): Int = bisect_right(ab, x, 0, ab.length)
  def bisect_left(ab:AB, x:Int):Int = bisect_left(ab, x, 0, ab.length)
  def bisect_left(ab: AB, target: Int, l: Int, r: Int): Int =
    if (l >= r) l else {
      val mid = l + (r - l) / 2
      if (ab(mid) < target) bisect_right(ab, target, mid + 1, r)
      else bisect_right(ab, target, l, mid)
    }
  def bisect_right(ab: AB, target: Int, l: Int, r: Int): Int =
    if (l >= r) l else {
      val mid = l + (r - l) / 2
      if (ab(mid) <= target) bisect_right(ab, target, mid + 1, r)
      else bisect_right(ab, target, l, mid)
    }

  def remove(ab: AB, x: Int): Unit = {
    val idx = bisect(ab, x) - 1
    if(ab(idx) == x) ab.remove(idx)
  }

  def find(ab: AB, x: Int): Int = {
    val idx = bisect(ab, x) -1
    if(ab(idx) == x) idx else -1
  }
}
