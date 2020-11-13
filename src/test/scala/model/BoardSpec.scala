package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardSpec extends AnyWordSpec with Matchers{
  "A board" when {
    "created properly but empty" should {
      val board = new Board
      "give access to its Stones" in {
        board.stone(0, 0) should be(Stone(0))
        board.stone(0, 1) should be(Stone(0))
        board.stone(0, 2) should be(Stone(0))
        board.stone(0, 3) should be(Stone(0))
        board.stone(0, 4) should be(Stone(0))
        board.stone(0, 5) should be(Stone(0))
        board.stone(0, 6) should be(Stone(0))
        board.stone(0, 7) should be(Stone(0))
        board.stone(1, 0) should be(Stone(0))
        board.stone(1, 1) should be(Stone(0))
        board.stone(1, 2) should be(Stone(0))
        board.stone(1, 3) should be(Stone(0))
        board.stone(1, 4) should be(Stone(0))
        board.stone(1, 5) should be(Stone(0))
        board.stone(1, 6) should be(Stone(0))
        board.stone(1, 7) should be(Stone(0))
        board.stone(2, 0) should be(Stone(0))
        board.stone(2, 1) should be(Stone(0))
        board.stone(2, 2) should be(Stone(0))
        board.stone(2, 3) should be(Stone(0))
        board.stone(2, 4) should be(Stone(0))
        board.stone(2, 5) should be(Stone(0))
        board.stone(2, 6) should be(Stone(0))
        board.stone(2, 7) should be(Stone(0))
      }
      "allow to set individual Stones and remain immutable" in {
        val changedBoard = board.update_board(0, 0, 1)
        changedBoard.stone(0, 0) should be(Stone(1))
        board.stone(0, 0) should be(Stone(0))
      }
      "allow to check individual Stones if set" in {
        val changedBoard = board.update_board(0, 0, 1)
        board.check_stone_Set(0, 0) should be(false)
        changedBoard.check_stone_Set(0, 0) should be(true)
      }
      "get amount of played stones depending on a specific color" in {
        val changedBoard = board.update_board(0, 0, 1)
        board.amount_of_played_stones(0) should be(24)
        changedBoard.amount_of_played_stones(1) should be(1)
      }
      "check if player achieved a 'mill'" in {
        //board.check_board_for_mill(1) should be(false)
        var oldBoard = board.update_board(0, 0, 1)
        var millBoard = oldBoard.update_board(0, 1, 1)
        millBoard = millBoard.update_board(0, 2, 1)
        millBoard.check_board_for_mill(oldBoard, 1) should be(true)
        millBoard.check_board_for_mill(oldBoard, 2) should be(false)

        oldBoard = oldBoard.update_board(0,1,1)
        oldBoard = oldBoard.update_board(0,2,1)
        millBoard.check_board_for_mill(oldBoard, 1) should be(false)
        
        millBoard = millBoard.update_board(0,1,0)
        millBoard.check_board_for_mill(oldBoard, 1) should be(false)

      }
    }
  }
}