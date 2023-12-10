import scala.io.Source
object `Day 2` {
  // 12r 13g  14 b

  def main2(args: String*) = {
    val filename = """src\main\scala\Day 2\input.txt"""
    val redPattern = """(\d+)\s+red""".r
    val bluePattern = """(\d+)\s+blue""".r
    val greenPattern = """(\d+)\s+green""".r

    val test = "331 yellow 32 red"
    val test2 = redPattern.findFirstIn(test)
    test2 match
      case Some(redPattern(number)) => println(number)
      case _                        => println("pattern not matched")

    val lines = Source.fromFile(filename).getLines.toList
    val sum = lines.zipWithIndex.map { (line, index) =>

      val validgames = line.split(";").forall { game =>
        val reds = redPattern.findFirstIn(game) match
          case Some(redPattern(number)) => number.toInt <= 12
          case _                        => true
        val blues = bluePattern.findFirstIn(game) match
          case Some(bluePattern(number)) => number.toInt <= 14
          case _                         => true
        val greens = greenPattern.findFirstIn(game) match
          case Some(greenPattern(number)) => number.toInt <= 13
          case _                          => true

        reds && blues && greens
      }
      validgames match
        case true =>
          index + 1
        case false => 0

    }.sum

    val powerSum = lines.map { line =>
      val games = line.split(";")
      val redMax = games.map { game =>
        redPattern.findFirstIn(game) match
          case Some(redPattern(number)) => number.toInt
          case _                        => 1
      }.max
      val blueMax = games.map { game =>
        bluePattern.findFirstIn(game) match
          case Some(bluePattern(number)) => number.toInt
          case _                         => 1
      }.max
      val greenMax = games.map{ game=>
        greenPattern.findFirstIn(game) match
            case Some(greenPattern(number))=> number.toInt
            case _ =>1
      }.max
      redMax*blueMax*greenMax
    }.sum
    println(sum)
    println(powerSum)
  }
}
