package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlayerSpec extends AnyWordSpec with Matchers{
  "A player" when {
    "new" should {
      val player = Player("Your name", 1, 9)
      "have a name" in {
        player.name should be("Your name")
      }
      "have a color" in {
        player.color should be(1)
      }
      "have stones a the beginning" in {
        player.MAX_STONE should be(9)
      }
    }
  }
}
