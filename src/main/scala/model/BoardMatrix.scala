package model

case class BoardMatrix[T](vectors: Vector[Vector[T]]) {
  def this(filling: T) = this(Vector.tabulate(3, 8) {(rectangle_num, position_num) => filling})

  def stone(rect_num: Int, pos_num: Int): T = vectors(rect_num)(pos_num)

  def replaceStone(rect_num: Int, pos_num:Int, stone: T): BoardMatrix[T] = {
    copy(vectors.updated(rect_num, vectors(rect_num).updated(pos_num, stone)))
  }

  def amountOfPlayedStones(color: Int): Int = {
    val flatVector = vectors.flatMap(i => i.map(j => j))
    val filteredFlatVector = flatVector.filter(i => i == Stone(color))
    filteredFlatVector.length
  }
}
