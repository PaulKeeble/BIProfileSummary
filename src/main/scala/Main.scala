import scala.io.Source
import java.io.File

object Main {
  def main(args:Array[String]) {
    val input = Source.fromFile(new File(args(0)))
    
    for(line <- input.getLines) {
      println(line)
    }
  }
}