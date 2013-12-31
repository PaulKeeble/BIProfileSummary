import scala.util.parsing.combinator.RegexParsers

class Timed

case class Entry(name:String,startTime:Double,runTime:Double,extra:String) extends Timed

case class ThreadEntry(name:String) extends Timed

object BIParser extends RegexParsers  {
  def number: Parser[Double] = """\d+\.\d*""".r ^^ { _.toDouble }
  
  def text: Parser[String] =  ".*".r
  
  def quotedText: Parser[String] = text
   
  def space: Parser[String] = " *".r
  
  def name: Parser[String] = "[a-zA-Z0-9#= _-]+".r
  
  def semi:Parser[String] = ";".r

  def extraInfo: Parser[String] = quotedText
  
  def startTime: Parser[Double] = space ~> number
  
  def runTime: Parser[Double] = space ~> number
  
  def entry: Parser[Entry] = (name <~ semi) ~ (startTime <~ semi) ~ (runTime <~ semi) ~ extraInfo ^^ {
    case name ~ startTime ~ runTime ~ extraInfo =>  Entry(name,startTime,runTime,extraInfo)
  }
  
  def threadStart: Parser[String] = literal("* Thread ")
  
  def thread: Parser[ThreadEntry] = (threadStart ~> name)  ^^ {
    case name  =>  ThreadEntry(name)
  }
  
  def line:Parser[Timed] = thread | entry
  
  def parse(in:String) = {
    try {
      parseAll(line,in).get
    } catch {
      case e:Exception => println("exceptioned on line: " + in); throw e
    }
  }
}