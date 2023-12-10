import scala.io.Source
type ID = Long
type IDRange = collection.IndexedSeqView[ID]
object `Day 5` { 
  def addToMap(
      destination: ID,
      source: ID,
      range: Int,
      map: collection.mutable.Map[IDRange, Long => Long]
  ) = {
    val key = (source until source + range).view
    val delta = destination - source
    val mapFunction: ID => ID = (x => delta + x)
    map.update(key, mapFunction)

  }
  def quickRangeContains(range: IDRange, value: Long) = {

    !range.isEmpty && range.head <= value && value <= range.last
  }
  def formatMap(input: String) = {
    input.split(" ").toList.tail.map(_.toLong).grouped(3).toList
  }
  @main def main5(args: String*) = {
    val filename = """src\main\scala\Day 5\input.txt"""
    val lines = Source.fromFile(filename).getLines.mkString
    val seedSplit = """seeds\:(\s*)"""
    val seedToSoilSplit = """seed-to-soil(\s*)map\:"""
    val soilToFertiliserSplit = """soil-to-fertilizer(\s*)map\:"""
    val fertilizerToWaterSplit = """fertilizer-to-water(\s*)map\:"""
    val waterToLightSplit = """water-to-light(\s*)map\:"""
    val lightToTempSplit = """light-to-temperature(\s*)map\:"""
    val tempToHumidity = """temperature-to-humidity(\s*)map\:"""
    val hummidityToLocation = """humidity-to-location(\s*)map\:"""
    val split =
      s"$seedSplit|$seedToSoilSplit|$soilToFertiliserSplit|$fertilizerToWaterSplit|$waterToLightSplit|$lightToTempSplit|$tempToHumidity|$hummidityToLocation".r
    val splitData = split.split(lines).toList.tail
    val seeds = splitData.head.split(" ").toList.map(_.toLong)
    val lines2 = Source.fromFile(filename).getLines.mkString("\n")
    val splitData2 = split.split(lines2).toList.tail

    val maps = splitData2.tail.map { string =>
      val formatted = formatMap(string.replace("\n"," "))

      val mapping = collection.mutable.Map.empty[IDRange, (ID => ID)]
      formatted.map {
        case List(destination, source, range) =>
          addToMap(destination, source, range.toInt, mapping)
        case _ => ()
      }
      mapping
    }

    val locations = seeds.map { seed =>
      val fromSeedToLocation = maps.foldLeft(seed) { (value, map) =>
        map.find((idRange, _) =>
          quickRangeContains(idRange, value)
        ) match // 14 14 53 14-> 53
          case Some(idRange, mapFunction) =>mapFunction(value)
          case None =>identity(value)

      }

      fromSeedToLocation
    }
    print(locations.min)

  }
}
