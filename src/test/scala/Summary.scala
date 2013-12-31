object Summary {

  case class SummedEntry(name:String, count:Long,totalTime:Double) {
    def +(that:SummedEntry) = copy(count = count + that.count, totalTime = totalTime + that.totalTime)
  }
  
  def reduceSum(a:SummedEntry,b:SummedEntry) = a + b
  
  def summarise(entries:List[Entry]) = {
    val sums = entries.map( e=>  SummedEntry(e.name,1,e.runTime))
    val byName = sums.groupBy(_.name)
    val reducedToSums = byName.mapValues( entries => entries.reduce(reduceSum) ).toList.map { case (name,entry) => entry}
        
    reducedToSums.sortBy(_.totalTime).reverse
  }
}