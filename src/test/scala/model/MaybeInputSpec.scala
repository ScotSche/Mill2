package model

import controller.Controller
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MaybeInputSpec extends AnyWordSpec with Matchers{
  "An Input" when {
    "new" should {
      val board = new Board
      val board1= board.update_board(1, 1,1)
      val board2= board1.update_board(1,0,2)
      val controller = new Controller(board, Vector())
      "check if the input length is two" in {
        MaybeInput(Some("IN")).validLength.input should be(Some("IN"))
        MaybeInput(Some("TOO LONG")).validLength.input should be(None)
        MaybeInput(None).validLength.input should be(None)
      }
      "check if the input consists of digits" in {
        MaybeInput(Some("123")).validInt.input should be(Some("123"))
        MaybeInput(Some("NaN")).validInt.input should be(None)
        MaybeInput(None).validInt.input should be(None)
      }
      "check if coordinates are valid and provide List[int]" in {
        MaybeInput(Some("11")).validCoordinates.input should be(Some(List(1, 1)))
        MaybeInput(Some("00")).validCoordinates.input should be(None)
        MaybeInput(Some("01")).validCoordinates.input should be(None)
        MaybeInput(Some("10")).validCoordinates.input should be(None)
        MaybeInput(Some("41")).validCoordinates.input should be(None)
        MaybeInput(Some("19")).validCoordinates.input should be(None)
        MaybeInput(Some("99")).validCoordinates.input should be(None)
        MaybeInput(None).validCoordinates.input should be(None)
      }
      "check if stone is already set in board" in {
        MaybeInput(Some(List(1, 1))).validateStone(controller.board).input should be(Some(List(1, 1)))
        controller.setStone(0, 0, 1)
        MaybeInput(Some(List(1, 1))).validateStone(controller.board).input should be(None)
        MaybeInput(None).validateStone(controller.board).input should be(None)
      }
      "return the possible moves to neighbor places on board" in{

        MaybeInput(Some(List(1,1))).findValidNeighbors(board2).input should be(Some(List((1,8), (1,2))))
        MaybeInput(Some(List(1,3))).findValidNeighbors(board2).input should be(Some(List((1,2), (1,4))))
        MaybeInput(Some(List(2,3))).findValidNeighbors(board2).input should be(Some(List((2,4))))
        MaybeInput(Some(List(3,2))).findValidNeighbors(board2).input should be(Some(List((3,1), (3,3))))
        MaybeInput(Some(List(2,2))).findValidNeighbors(board2).input should be(Some(List((2,3), (3,2), (1,2))))
        MaybeInput(Some(List(1,9))).findValidNeighbors(board2).input should be(None)
        MaybeInput(None).findValidNeighbors(board2).input should be(None)
      }
      "return the possible moves in the rectangle" in{
        MaybeInput(Some(List(1,1))).legal_moves_in_rectangle(List(0,0)) should be (List((0,7), (0,1)))
      }
      "return the possible moves between the rectangles" in{
        MaybeInput(Some(List(2,2))).legal_moves_between_rectangles(List(1,1)) should be (List((2,1), (0,1)))
      }
      "return stone positions, when stone is of player" in{
        MaybeInput(Some(List(2,2))).checkStone(board2, 1).input should be(Some(List(2,2)))
        MaybeInput(Some(List(2,1))).checkStone(board2, 2).input should be(Some(List(2,1)))
        MaybeInput(Some(List(2,1))).checkStone(board2, 1).input should be(None)
        MaybeInput(None).checkStone(board2, 1).input should be(None)

      }
      "check if stone position is set by competitor stone" in{
        MaybeInput(Some(List(2,1))).checkCompStone(board2, controller, 1).input should be(Some(List(2,1)))
        MaybeInput(Some(List(2,1))).checkCompStone(board2, controller, 2).input should be(None)
        MaybeInput(None).checkCompStone(board2, controller, 1).input should be(None)
      }
    }
  }
}
