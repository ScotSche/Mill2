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
            copy(Success((rect_num, pos_num)))
          }
        }
      }
      else copy(Failure(new InputException("Invalid integer characters")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validateCoordinatesOnBoard: InputHandlerPattern = input match {
    case Success(value: (Int, Int)) => {
      if((value._1 > 0 && value._1 < 4) && (value._2 > 0 && value._2 < 9)) copy(Success(value))
      else copy(Failure(new InputException("No valid coordinates")))
    }
    case Failure(exception) => copy(Failure(exception))
  }
  def validateStonePosition(board: Board): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) => if ( !board.check_stone_Set(value._1 - 1, value._2 - 1)) copy(Success(value))
    else copy(Failure(new InputException("Stone position is already used")))
    case Failure(exception) => copy(Failure(exception))
  }
  def validateOwnPlayerStone(board: Board, color: Int): InputHandlerPattern = input match {
    case Success(value: (Int, Int)) =>
      if (board.stone((value._1 - 1), (value._2 - 1)).color == color) copy(Success(value))
      else copy(Failure(new InputException("Chosen Stone is not your own")))
    case Failure(exception) => copy(Failure(exception))
  }

  class InputException(msg: String) extends Exception(msg){}
}
