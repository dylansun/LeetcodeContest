// TODO: under construction
case class Range (left:Int, right:Int){
  def +(that:Range):Range = Range(this.left min that.left, this.right max that.right)
  def -(that:Range):Option[Range] = {
    if(this.left >= that.left && this.right <= that.right) None
    else if(this.right <= that.left || this.left >= that.right) Some(this)
    else if(this.left >= that.left && this.right > that.right) Some(Range(that.right, this.right))
    else Some(Range(this.left min that.left, this.right min that.left))
  }
  def intersect(that:Range):Boolean = {
    (that.right > this.left && that.right <= this.right) ||
      (that.left >= this.left && that.left < this.right)
  }
}
