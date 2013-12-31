import scala.util.parsing.combinator.RegexParsers

class Timed(name:String,startTime:Double,runTime:Double,extra:String)

case class Entry(name:String,startTime:Double,runTime:Double,extra:String) extends Timed(name,startTime,runTime,extra)

case class ThreadEntry(name:String,startTime:Double,runTime:Double,extra:String) extends Timed(name,startTime,runTime,extra)

object BIParser extends RegexParsers  {
  def number: Parser[Double] = """\d+\.\d*""".r ^^ { _.toDouble }
  
  def quote: Parser[String] = "\"".r
  
  //matches all characters except " for quoted strings
  def text: Parser[String] =  "[^\"]*".r
  
  def quotedText: Parser[String] = quote ~> text <~ quote
   
  def space: Parser[String] = " *".r
  
  def name: Parser[String] = "[a-zA-Z0-9 ]+".r
  
  def semi:Parser[String] = ";".r

  def extraInfo: Parser[String] = quotedText
  
  def startTime: Parser[Double] = space ~> number
  
  def runTime: Parser[Double] = space ~> number
  
  def entry: Parser[Entry] = (name <~ semi) ~ (startTime <~ semi) ~ (runTime <~ semi) ~ extraInfo ^^ {
    case name ~ startTime ~ runTime ~ extraInfo =>  Entry(name,startTime,runTime,extraInfo)
  }
  
  def threadStart: Parser[String] = literal("* Thread ")
  
  def thread: Parser[ThreadEntry] = (threadStart ~> name <~ semi) ~ (startTime <~ semi) ~ (runTime <~ semi) ~ extraInfo ^^ {
    case name ~ startTime ~ runTime ~ extraInfo =>  ThreadEntry(name,startTime,runTime,extraInfo)
  }
  
  def line:Parser[Timed] = thread | entry
}