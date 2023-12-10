import scala.io.Source
case class point(row: Int, col: Int, maxRow: Int, maxCol: Int) {
  def down = this.copy(row = row + 1)
  def right = this.copy(col = col + 1)
  def up = this.copy(row = row - 1)
  def left = this.copy(col = col - 1)
  def diagDR = down.right
  def diagUR = up.right
  def diagDL = down.left
  def diagUL = up.left
  def valid = 0 <= row && row <= maxRow && 0 <= col && col <= maxCol
}
object `Day 3` {

  @main def main3(args: String*) = {
    val x = "123djwiad234"


    val filename = """src\main\scala\Day 3\input.txt"""
    val lines = Source.fromFile(filename).getLines.toList.zipWithIndex
    val isPart = (x: Char) => x != '.' && !(x.isDigit)

    val characters = lines.map((line, index) =>
      line.toList zip LazyList.continually(index).zip(LazyList.from(0))
    )
    val numbers = characters.map{line=>

    }
    println(characters)
  }
}
