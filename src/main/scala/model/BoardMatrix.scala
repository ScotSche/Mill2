package model

case class BoardMatrix[T](rows: Vector[Vector[T]]) {
  def this(filling: T) = this(Vector.tabulate(3, 8) {(rectangle_num, position_num) => filling})

  def stone(rect_num: Int, pos_num: Int): T = rows(rect_num)(pos_num)

  def replaceStone(rect_num: Int, pos_num:Int, stone: T): BoardMatrix[T] = {
    copy(rows.updated(rect_num, rows(rect_num).updated(pos_num, stone)))
  }

  def amountOfPlayedStones(color: Int): Int = {
    val flatVector = rows.flatMap(i => i.map(j => j))
    val filteredFlatVector = flatVector.filter(i => i == Stone(color))
    filteredFlatVector.length
  }
}
