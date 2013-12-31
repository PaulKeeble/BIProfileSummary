import scala.io.Source
import java.io.File
import scala.collection.mutable

object Main {
  def main(args:Array[String]) {
    val input = Source.fromFile(new File(args(0)))

    val parsedLines = for(line <- input.getLines) yield BIParser.parse(line) 
    
    val threadsToEntries = entryMap(parsedLines)
    
    for((thread,entries) <- threadsToEntries) {
      
      println(thread.name)
      val summaryEntries = Summary.summarise(entries)
      
      summaryEntries.foreach { e =>
        println(e.name +" times=" + e.count+" totalTime="+ e.totalTime) 
      }
      println
      println
    }
    
  }
  
  // Converts stream of timed to threads mapped to Entries for that thread
  def entryMap(parsedLines:Iterator[Timed]) = {
    var thread :ThreadEntry = null
    val threads: mutable.Map[ThreadEntry,List[Entry]] = mutable.Map()
    
    for(e <- parsedLines) {
      e match {
        case te : ThreadEntry => {
          thread = te
          threads.put(thread,List())
        }
        case e : Entry => threads put(thread,e :: threads(thread))
      }
    }
    
    val inOrder = for((k,v) <- threads) yield (k,v.reverse)
    inOrder.toMap
  }
}