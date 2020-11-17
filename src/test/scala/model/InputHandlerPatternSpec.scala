package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success, Try}

class InputHandlerPatternSpec extends AnyWordSpec with Matchers{
  "An input" when{
    "new" should{
      var board = new Board

      "check if the input length is two" in {
        val success = Try(InputHandlerPattern(Success("11")).validateInputLength.input).get
        success should be(Success("11"))
        val error = Try(InputHandlerPattern(Success("Invalid")).validateInputLength.input).get
        error match {
          case Failure(exception) => exception.getMessage should be("Invalid amount of characters")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateInputLength.input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if the input consists of integers" in {
        val success = Try(InputHandlerPattern(Success("11")).validateCharactersAsInt.input).get
        success should be(Success((0, 0)))
        val error = Try(InputHandlerPattern(Success("1b")).validateCharactersAsInt.input).get
        error match {
          case Failure(exception) => exception.getMessage should be("Invalid integer characters")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateCharactersAsInt.input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if the coordinates are a valid input" in {
        val success = Try(InputHandlerPattern(Success((0, 0))).validateCoordinatesOnBoard.input).get
        success should be(Success((0, 0)))
        val error1 = Try(InputHandlerPattern(Success((0, 8))).validateCoordinatesOnBoard.input).get
        error1 match {
          case Failure(exception) => exception.getMessage should be("No valid coordinates")
        }
        val error2 = Try(InputHandlerPattern(Success((3, 0))).validateCoordinatesOnBoard.input).get
        error2 match {
          case Failure(exception) => exception.getMessage should be("No valid coordinates")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateCoordinatesOnBoard.input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if chosen stone position is already used" in {
        val success = Try(InputHandlerPattern(Success((0, 0))).validateStonePosition(board).input).get
        success should be(Success((0, 0)))
        board = board.update_board(0, 0, 1)
        val error = Try(InputHandlerPattern(Success((0, 0))).validateStonePosition(board).input).get
        error match {
          case Failure(exception) => exception.getMessage should be("Stone position is already used")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateStonePosition(board).input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if selected stone is own player stone" in {
        val success = Try(InputHandlerPattern(Success((0, 0))).validateOwnPlayerStone(board, 1).input).get
        success should be(Success((0, 0)))
        board = board.update_board(0, 1, 2)
        val error1 = Try(InputHandlerPattern(Success((0, 1))).validateOwnPlayerStone(board, 1).input).get
        error1 match {
          case Failure(exception) => exception.getMessage should be("Chosen Stone is not your own")
        }
        val error2 = Try(InputHandlerPattern(Success((0, 2))).validateOwnPlayerStone(board, 1).input).get
        error2 match {
          case Failure(exception) => exception.getMessage should be("Chosen Stone is not your own")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateOwnPlayerStone(board, 1).input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if stone has valid neighbours for 'Stone-Move'" in {
        val success1 = Try(InputHandlerPattern(Success((0, 0))).validatePossibleNeighbourStones(board).input).get
        success1 should be(Success(List((0, 0), (0, 7))))

        board = board.update_board(0, 3, 1)
        val success2 = Try(InputHandlerPattern(Success((0, 3))).validatePossibleNeighbourStones(board).input).get
        success2 should be(Success(List((0, 3), (0, 2), (0, 4), (1, 3))))

        board = board.update_board(0, 3, 0)
        board = board.update_board(1, 3, 1)
        val success3 = Try(InputHandlerPattern(Success((1, 3))).validatePossibleNeighbourStones(board).input).get
        success3 should be(Success(List((1, 3), (1, 2), (1, 4), (2, 3), (0, 3))))

        board = board.update_board(1, 3, 0)
        board = board.update_board(2, 3, 1)
        val success4 = Try(InputHandlerPattern(Success((2, 3))).validatePossibleNeighbourStones(board).input).get
        success4 should be(Success(List((2, 3), (2, 2), (2, 4), (1, 3))))
        board = board.update_board(2, 3, 0)
        board = board.update_board(0, 7, 2)
        val error = Try(InputHandlerPattern(Success((0, 0))).validatePossibleNeighbourStones(board).input).get
        error match {
          case Failure(exception) => exception.getMessage should be("There are no possible moves for this stone")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validatePossibleNeighbourStones(board).input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }

      "check if input matches with one possible neighbour" in {
        board = board.update_board(0, 7, 0)
        val success = Try(InputHandlerPattern(Success((0, 7))).validateNeighboursWithInput(List((0, 0), (0, 7))).input).get
        success should be(Success((0, 7)))
        val error = Try(InputHandlerPattern(Success((0, 6))).validateNeighboursWithInput(List((0, 0), (0, 7))).input).get
        error match {
          case Failure(exception) => exception.getMessage should be("Input does not match a possible stone position")
        }
        val errorRail = Try(InputHandlerPattern(Failure(new Exception("Failed"))).validateNeighboursWithInput(List((0, 0), (0, 7))).input).get
        errorRail match {
          case Failure(exception) => exception.getMessage should be("Failed")
        }
      }
    }
  }
}
