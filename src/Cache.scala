import scala.collection.mutable.{HashMap => HM}
class Cache[A,B] {
  val mem = HM[A,B]()
  def cache(f: A=>B, input:A):B = {
    if (mem.contains(input)) mem(input)
    else {mem.put(input, f(input));mem(input)}
  }
}


