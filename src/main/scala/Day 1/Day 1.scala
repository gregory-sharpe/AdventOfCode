import scala.io.Source

object `Day 1` {
  
  @main
  def main1(args: String*) = {
    val filename = """src\main\scala\Day 1\Day1.txt"""
    val lines = Source.fromFile(filename).getLines.toList
    val sum = lines.map{line=>
    val newLine =line.toLowerCase().replace("one","one1one").replace("two","two2two").
      replace("three","three3three").replace("four","four4four").
      replace("five","five5five").replace("six","six6six").
      replace("seven","seven7seven").replace("eight","eight8eight").replace("nine","nine9nine")// Extrmely inneficient
      val first = newLine.find(Character.isDigit(_)).map(Character.getNumericValue(_)).getOrElse(0)
      val last =  newLine.findLast(Character.isDigit(_)).map(Character.getNumericValue(_)).getOrElse(0)
      first*10+last
    }.sum
    print(sum)
}
}