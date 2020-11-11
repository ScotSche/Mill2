package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardMatrixSpec extends AnyWordSpec with Matchers{
  "A BoardMatrix is a tailor-made immutable data type that contains a three-dimensional Vector of Stones. " +
    "A BoardMatrix" when {
    "replace stones and return a new data structure" in {
      val boardMatrix = new BoardMatrix[Stone](Stone(0))
      val returnedBoardMatrix = boardMatrix.replaceStone(0, 0, Stone(1))
      boardMatrix.stone(0, 0) should be(Stone(0))
      returnedBoardMatrix.stone(0, 0) should be(Stone(1))
    }
    "show amount of played stones" in {
      val board = new BoardMatrix[Stone](Stone(0))
      board.amountOfPlayedStones(0) should be(24)
    }
  }
}
