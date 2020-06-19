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
  def main(args: Array[String]): Unit = {

    val c_text = List( "#  Weekly Contests Solutions with Scala ",
      "-------------------------------",
      "|  Title | code | report| ",
      "| :-----: | :--------: | :----------: |")
    val writer = new PrintWriter(new File("README.md"))
    for(t <- c_text) writer.println(t)

    for{
      cid <- 193 to 83 by -1
      start = if(cid < 10) 1 else cid / 10 * 10
      finish = if(cid < 10) 9 else start + 9
      dir_name = "C" + start + "_"+finish
      code_name = "C"+cid+".scala"
      report_name = "C" + cid + ".md"
      code_file = "src/"+dir_name + "/" + code_name
      report_file = "contest_report/"+dir_name+"/" + report_name
    }{
      writer.println(s"|第 $cid 场周赛|[Scala](${code_file})|[md]($report_file)|")
    }

    for{
      cid <- 28 to 1 by -1
      start = if(cid < 10) 1 else cid / 10 * 10
      finish = if(cid < 10) 9 else start + 9
      dir_name = "B" + start + "_"+finish
      code_name = "B"+cid+".scala"
      report_name = "B" + cid + ".md"
      code_file = "src/"+dir_name + "/" + code_name
      report_file = "contest_report/"+dir_name+"/" + report_name
    }{
      writer.println(s"|第 $cid 场双周赛|[Scala]($code_file)|[md]($report_file)|")
    }

    Source.fromFile("ReadMe_footer.md").getLines() foreach {
      line => writer.println(line)
    }
    writer.close()
  }

}
