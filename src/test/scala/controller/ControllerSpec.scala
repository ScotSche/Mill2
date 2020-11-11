package controller

import model.Board
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.Observer

class ControllerSpec extends AnyWordSpec with Matchers{
  "A Controller" when {
    "observed by a Observer" should {
      val board = new Board
      val controller = new Controller(board, Vector())
      val observer = new Observer {
        var updated: Boolean = false
        def isUpdated:Boolean = updated
        override def update: Unit = updated = true

        var playerupdated: Boolean = false;
        def isPlayerUpdated:Boolean = playerupdated
        override def updatePlayer: Unit = playerupdated = true
      }
      controller.add(observer)
      "notify its Observer after creation" in {
        controller.create_empty_Board()
        observer.updated should be(true)
      }
      "notify its Observer after setting a stone" in {
        controller.setStone(0, 0, 1)
        observer.updated should be(true)
        controller.board.stone(0, 0).color should be(1)
      }
      "check if input coordinates are valid" in {
        controller.checkInputCoordinates(0, 0) should be(false)
        controller.checkInputCoordinates(0, 1) should be(false)
        controller.checkInputCoordinates(1, 0) should be(false)
        controller.checkInputCoordinates(1, 9) should be(false)
        controller.checkInputCoordinates(4, 1) should be(false)
        controller.checkInputCoordinates(1, 1) should be(true)
      }
      "show amount of stones on the board" in {
        controller.amountOfPlayerStones(1) should be(1)
      }
      "create new players" in {
        controller.create_new_Players("Name 1", "Name 2")
        controller.players(0).name should be("Name 1")
        controller.players(0).color should be(1)
        controller.players(1).name should be("Name 2")
        controller.players(1).color should be(2)
      }
      "check if stone is set" in {
        controller.checkStoneSet(0, 0) should be(true)
        controller.checkStoneSet(0, 1) should be(false)
      }
      "remove specific competitor stone" in {
        controller.remove_stone(0, 0, 2) should be(true)
        controller.remove_stone(0, 0, 1) should be(false)
        controller.remove_stone(0, 1, 1) should be(false)
      }
    }
  }
}
