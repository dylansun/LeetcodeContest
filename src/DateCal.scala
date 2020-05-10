object DateCal {
  // 1971-1-1, Friday
  val names = Array("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  val days = Array(0,31,28,31,30,31,30,31,31,30,31,30,31)
  def isLeap(year:Int):Boolean = if(year % 100 == 0) (year % 400) == 0 else year % 4 == 0
  def bool2int(p:Boolean):Int = if(p) 1 else 0
  def dayOfWeek(year:Int, month:Int, day:Int):Int = {
    var x = 5
    for{ year  <- 1971 until year} x = (x + 1 + bool2int(isLeap(year))) % 7
    for{ i <- 1 until month} x = (x + days(i) ) % 7
    if(month > 2 && isLeap(year)) x = (x + 1) % 7
    for {i <- 1 until day } x = (x + 1) % 7
    x
  }
  def dayOfYear(year:Int, month:Int, day:Int):Int = {
    days.slice(0,month).sum + day + bool2int(month > 2 && isLeap(year))
  }
}
