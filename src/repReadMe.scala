import scala.collection.mutable
import java.io.PrintWriter
import java.io.File

object repReadMe {
  val Source = scala.io.Source
  def subDir(dir: File): Iterator[File] = {
    val dirs = dir.listFiles().filter(_.isDirectory)
    val files = dir.listFiles().filter(_.isFile)
    files.toIterator ++ dirs.toIterator.flatMap(subDir)
  }
  def filesOfDir(dir: File): Iterator[File] = {
    dir.listFiles().filter(_.isFile).toIterator
  }
  def filesOfDir(dir: String): Iterator[File] = filesOfDir(new File(dir))

  def testFilesOfDir():Unit = {
    filesOfDir(new File("report/")) foreach println
  }
  def testSubDir():Unit = {
    subDir(new File(".")) foreach println
  }
  def fileExist(path:String):Boolean = new File(path).exists()
  def main(args: Array[String]): Unit = {
    val last_contest = 201
    val last_bicoontest = 32
    val c_text = List( "#  Weekly Contests Solutions ",
      "-------------------------------",
      "|  Title | Scala | Python 3 |Report| ",
      "| :-----: | :--------: | :--------: | :----------: |")
    val writer = new PrintWriter(new File("README.md"))
    for(t <- c_text) writer.println(t)

    for{
      cid <- last_contest to 83 by -1
      start = if(cid < 10) 1 else cid / 10 * 10
      finish = if(cid < 10) 9 else start + 9
      dir_name = "C" + start + "_"+finish
      code_name = "C"+cid+".scala"
      py3_code_name= "C"+cid+".py"
      report_name = "C" + cid + ".md"
      scala_path = "src/"+dir_name + "/" + code_name
      report_path = "contest_report/"+dir_name+"/" + report_name
      py3_path = "src_python3/"+dir_name + "/" + py3_code_name
    }{
      val scala_file = if(fileExist(scala_path)) "[Scala]("+ scala_path+")" else " "
      val md_file =  if(fileExist(report_path)) "[md]("+ report_path +")"else " "
      val py3_file = if(fileExist(py3_path)) "[py3]("+py3_path+")" else " "
      if(scala_file != " " || md_file !=" "  || py3_file != " "  )
      writer.println(s"|第 $cid 场周赛|${scala_file}|${py3_file}|${md_file}|")
    }

    for{
      cid <- last_bicoontest to 1 by -1
      start = if(cid < 10) 1 else cid / 10 * 10
      finish = if(cid < 10) 9 else start + 9
      dir_name = "B" + start + "_"+finish
      code_name = "B"+cid+".scala"
      py3_code_name = "B"+cid+".py"
      report_name = "B" + cid + ".md"
      scala_path = "src/"+dir_name + "/" + code_name
      report_path = "contest_report/"+dir_name+"/" + report_name
      py3_path = "src_python3/"+dir_name + "/" + py3_code_name
    }{
      val scala_file = if(fileExist(scala_path)) "[Scala]("+ scala_path+")" else " "
      val md_file =  if(fileExist(report_path)) "[md]("+ report_path +")"else " "
      val py3_file = if(fileExist(py3_path)) "[py3]("+py3_path+")" else " "
      println(s"Process Bi-contest ${cid}")
      println(s"Found scala src:${fileExist(scala_path)}")
      println(s"Found py3 src:${fileExist(py3_path)}")
      println(s"Found md src:${fileExist(report_path)}")
      if(scala_file != " " || md_file !=" "  || py3_file != " "  )
      writer.println(s"|第 $cid 场双周赛|${scala_file}|${py3_file}|${md_file}|")
    }

    Source.fromFile("ReadMe_footer.md").getLines() foreach {
      line => writer.println(line)
    }
    writer.close()
  }

}
