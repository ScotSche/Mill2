package model

import scala.util.{Failure, Success, Try}

case class InputHandlerPattern(input: Try[Any]) {
  def validateInputLength: InputHandlerPattern = input match {
    case Success(value: String) => if(value.length == 2) copy(Success(value)) else copy(Failure(new InputException("Invalid amount of characters")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validateCharactersAsInt: InputHandlerPattern = input match {
    case Success(value: String) =>
      if(value forall Character.isDigit) {
        value.toList.filter(c => c != ' ').map(c => c.toString.toInt) match {
          case rect_num :: pos_num :: Nil => {
            copy(Success((rect_num - 1, pos_num - 1)))
          }
        }
      }
      else copy(Failure(new InputException("Invalid integer characters")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validateCoordinatesOnBoard: InputHandlerPattern = input match {
    case Success(value: (Int, Int)) => {
      if((value._1 >= 0 && value._1 < 3) && (value._2 >= 0 && value._2 < 8)) copy(Success(value))
      else copy(Failure(new InputException("No valid coordinates")))
    }
    case Failure(exception) => copy(Failure(exception))
  }
  def validateStonePosition(board: Board): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) => if ( !board.check_stone_Set(value._1, value._2)) copy(Success(value))
    else copy(Failure(new InputException("Stone position is already used")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validateOwnPlayerStone(board: Board, color: Int): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) =>
      if (board.stone(value._1, value._2).color == color) copy(Success(value))
      else copy(Failure(new InputException("Chosen Stone is not your own")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validatePossibleNeighbourStones(board: Board): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) =>
      // In specific corner
      val neighbourList = List((value._1, if(value._2 - 1 < 0) 7 else value._2 - 1), (value._1, if(value._2 + 1 > 7) 0 else value._2 + 1))
      if(value._2 % 2 == 0){
        val validNeighbours = neighbourList.filter((value: (Int, Int)) => !board.check_stone_Set(value._1, value._2))
        if( !validNeighbours.isEmpty) copy(Success(value :: validNeighbours)) else copy(Failure(new InputException("There are no possible moves for this stone")))
      }
      // Middle Stones
      else{
        val additionalNeighbourList = neighbourList ++ (value._1 match {
          case 0 => List((value._1 + 1, value._2))
          case 1 => List((value._1 + 1, value._2), (value._1 - 1, value._2))
          case 2 => List((value._1 - 1, value._2))
        })
        val validNeighbours = additionalNeighbourList.filter((value: (Int, Int)) => !board.check_stone_Set(value._1, value._2))
        if( !validNeighbours.isEmpty) copy(Success(value :: validNeighbours)) else copy(Failure(new InputException("There are no possible moves for this stone")))
      }
    case Failure(exception) => copy(Failure(exception))
  }
  def validateNeighboursWithInput(neighbours: List[(Int, Int)]): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) =>
      val result = neighbours.filter((i: (Int, Int)) => (i._1 == value._1) && (i._2 == value._2))
      if( !result.isEmpty) copy(Success(value)) else copy(Failure(new InputException("Input does not match a possible stone position")))

    case Failure(exception) => copy(Failure(exception))
  }

  class InputException(msg: String) extends Exception(msg){}
}
