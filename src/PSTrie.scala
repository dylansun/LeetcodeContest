import scala.collection.mutable.{HashMap => HM}
class PSTrie(){
  val childs = HM[(Char, Char), PSTrie]()
  var _weight = -1
  type L = List[Char]
  def insert(l:L,rl:L, weight:Int):Unit = {
    _weight = weight max _weight
    (l,rl) match {
      case (h1::t1, h2::t2) =>
        this.insert_prefix(l, weight)
        this.insert_suffix(rl, weight)
        if(!this.childs.contains((h1,h2))) this.childs.put((h1,h2), new PSTrie)
        this.childs((h1,h2)).insert(t1,t2,weight)
      case (_,_) =>{}
    }
  }
  def insert_prefix(l:L, weight:Int):Unit = {
    _weight = _weight max weight
    l match {
      case Nil => {}
      case h::t =>
        if(!this.childs.contains((h,' '))) this.childs.put((h, ' '), new PSTrie())
        this.childs((h, ' ')).insert_prefix(t, weight)
    }
  }
  def insert_suffix(l:L, weight:Int):Unit = {
    _weight = _weight max weight
    l match {
      case Nil => {}
      case h::t =>
        if(!this.childs.contains((' ',h))) this.childs.put((' ', h), new PSTrie())
        this.childs((' ', h)).insert_suffix(t, weight)
    }
  }
  def search(l:List[(Char, Char)]):Int = l match {
    case Nil => println("Found");_weight
    case h::t => if(this.childs.contains(h)) childs(h).search(t) else -1
  }
  def printKey():Unit = {
    childs foreach println
  }
}