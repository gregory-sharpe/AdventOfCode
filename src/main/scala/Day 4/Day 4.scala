import scala.io.Source

object `Day 4` {
  @main def main4(args: String*) ={
    val filename = """src\main\scala\Day 4\input.txt"""
    val lines = Source.fromFile(filename).getLines.toList
    val numbers = """(\d+)""".r
    val leading = """Card(\s*)(\d+)\:""".r
    val sum = lines.map{ line=>
        val partition = line.split("""\|""",2)
        val winningNumbers =numbers.findAllIn( leading.replaceAllIn(partition(0),"")).toSet
        val myNumbers = numbers.findAllIn(partition(1)).toSet
        val intersection = myNumbers.intersect(winningNumbers)
        intersection.size match
            case 0 => 0
            case matches => math.pow(2,matches-1).toInt
    }.sum
    println(sum)

    val cardWinningsMap: collection.mutable.Map[Int,Int] = collection.mutable.Map.empty[Int,Int]
    val extentsion = lines.reverse.zipWithIndex.map{(line,index)=>
        val partition = line.split("""\|""",2)
        val winningNumbers =numbers.findAllIn( leading.replaceAllIn(partition(0),"")).toSet// not the amount of each card.
        // but the value of each card. the last card in the input will always have a value of 1.
        val myNumbers = numbers.findAllIn(partition(1)).toSet
        val numberOfIntersections = myNumbers.intersect(winningNumbers).size
        cardWinningsMap.update(index,1)
        val min = math.max((index-numberOfIntersections),0)
        for (previousWinnings<- (min) until index){
            val previousValue = cardWinningsMap.getOrElse(previousWinnings,0)
            val currentValue = cardWinningsMap.getOrElse(index,0)
            cardWinningsMap.update(index,currentValue+previousValue)
        }

    }
    println(cardWinningsMap)
    println(cardWinningsMap.values.sum)

    }
    

  }

