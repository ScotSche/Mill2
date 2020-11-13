package model

case class Board(stones: BoardMatrix[Stone]) {

  def this() = this(new BoardMatrix[Stone](Stone(0)))

  def stone(rect_num: Int, pos_num: Int) = stones.stone(rect_num, pos_num)

  def update_board(rectangle_num: Int, position_num:Int, color:Int) : Board ={
    copy(stones.replaceStone(rectangle_num, position_num, Stone(color)))
  }

  def check_stone_Set(rectangle_num: Int, position_num:Int): Boolean ={
    stones.stone(rectangle_num, position_num).isSet
  }

  def amount_of_played_stones(color: Int): Int = {
    stones.amountOfPlayedStones(color)
  }

  def setup_relevant_Stones(in: Stone, color: Int): String = {
    if(in.color == color) {"1"}
    else {"0"}
  }

  def vecToString(vec: Vector[String]): String = vec.mkString


  def check_board_for_mill(oldBoard: Board, color: Int): Boolean = {
    val millControlVector = Vector(
      Integer.parseInt("111000000000000000000000", 2),
      Integer.parseInt("001110000000000000000000", 2),
      Integer.parseInt("000011100000000000000000", 2),
      Integer.parseInt("100000110000000000000000", 2),
      Integer.parseInt("000000001110000000000000", 2),
      Integer.parseInt("000000000011100000000000", 2),
      Integer.parseInt("000000000000111000000000", 2),
      Integer.parseInt("000000001000001100000000", 2),
      Integer.parseInt("000000000000000011100000", 2),
      Integer.parseInt("000000000000000000111000", 2),
      Integer.parseInt("000000000000000000001110", 2),
      Integer.parseInt("000000000000000010000011", 2),
      Integer.parseInt("010000000100000001000000", 2),
      Integer.parseInt("000100000001000000010000", 2),
      Integer.parseInt("000001000000010000000100", 2),
      Integer.parseInt("000000010000000100000001", 2))

    val oldBoardVector = oldBoard.stones.rows.map(i => i.map(j => setup_relevant_Stones(j, color)))
    val oldBoardIntFlatVector = Integer.parseInt(vecToString(oldBoardVector.flatMap(i => i.map(j => j.mkString))), 2)

    val relevantVector = stones.rows.map(i => i.map(j => setup_relevant_Stones(j, color)))
    val relevantIntFlatVector = Integer.parseInt(vecToString(relevantVector.flatMap(i => i.map(j => j.mkString))), 2)

    val oldMills = millControlVector.filter(i => (i & oldBoardIntFlatVector) == i)
    val newMills = millControlVector.filter(i => (i & relevantIntFlatVector) == i)

    val newMillCheck = newMills.map(n => if (oldMills.contains(n)) false else true)

    if (newMillCheck.contains(true)) true else false
  }
}
