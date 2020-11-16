package view

import controller.{Controller, GameStatus}
import model.{Board, MaybeInput, Player, Stone}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class TuiSpec extends AnyWordSpec with Matchers{

  "A Mill Tui" should{
    val controller = new Controller(new Board, Vector())
    controller.create_new_Players("PlayerOne", "PlayerTwo")
    val tui = new Tui(controller)
    val board = new Board
    val board1= board.update_board(1, 1,1)
    val board2= board1.update_board(1,0,2)
    val board3= board2.update_board(1, 2,1)
    val board4= board3.update_board(1, 3,1)
    "create a new board on input 'n'" in{
      tui.processInputLine("n")
      controller.board should be(new Board)
    }
    "should do nothing and leave loop on input 'q'" in {
      tui.processInputLine("q")
    }
    "should print the help board on input 'h'" in {
      tui.processInputLine("h")
    }
    "should provide any other input" in {
      tui.processInputLine("*")
    }


    "should do nothing and leave loop in input 'q' in game mode" in {
      tui.processGameInputLine("q")
    }
    "should print the help board on input 'h' in game mode" in {
      tui.processGameInputLine("h")
    }

    "should handle a valid input in GPONE" in {
      controller.gameStatus = GameStatus.GPONE
      controller.create_empty_Board()
      tui.processGameInputLine("11")
      controller.board.stone(0, 0) should be(Stone(1))

      tui.processGameInputLine("12")
      controller.board.stone(0, 1) should be(Stone(2))
    }
    "should handle an invalid input in GPONE" in {
      controller.gameStatus = GameStatus.GPONE
      tui.processGameInputLine("Invalid")
    }
    "should handle a valid mill input in GPONE" in {
      tui.processGameInputLine("18")
      tui.processGameInputLine("13")
      tui.processGameInputLine("17")
      tui.newMill should be(true)
      tui.processGameInputLine("12")
      controller.board.check_stone_Set(0, 1) should be(false)
    }
    "should change from GPONE to GPTWO" in {
      tui.processGameInputLine("12")
      tui.processGameInputLine("14")
      tui.processGameInputLine("15")
      tui.processGameInputLine("16")
      tui.processGameInputLine("21")
      tui.processGameInputLine("22")
      tui.processGameInputLine("23")
      tui.processGameInputLine("24")
      tui.processGameInputLine("25")
      tui.processGameInputLine("26")
      tui.processGameInputLine("27")
      tui.processGameInputLine("28")
      tui.processGameInputLine("31")

      controller.gameStatus should be(GameStatus.GPTWO)
    }

    "should handle a valid input in GPTWO" in {
      tui.processGameInputLine("11")
      tui.processGameInputLine("32")

      controller.board.stone(0, 0) should be(Stone(0))
      controller.board.stone(2, 1) should be(Stone(1))
    }
    "should handle an invalid input in GPTWO" in {
      tui.gpTwoSeparator = false
      tui.processGameInputLine("Invalid")

      tui.gpTwoSeparator = true
      tui.processGameInputLine("Invalid")
      tui.gpTwoSeparator = false
    }
    "should handle a mill input in GPTWO" in {
      // Player Two Stone move
      tui.processGameInputLine("12")
      tui.processGameInputLine("33")
      // Player One Stone move
      tui.processGameInputLine("32")
      tui.processGameInputLine("11")

      tui.newMill should be(true)
      // Player One Invalid Remove Stone input
      tui.processGameInputLine("Invalid")
      tui.newMill should be(true)
      // Player One Remove Competitor Stone
      tui.processGameInputLine("33")
    }
    "should change from GPTWO to GPTHREE" in {
      controller.players(0).MAX_STONE = 3
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.GPTHREE)
    }

    "should handle a valid input in GPTHREE" in {
      tui.processGameInputLine("14")
      tui.processGameInputLine("12")

      controller.board.stone(0, 3) should be(Stone(0))
      controller.board.stone(0, 1) should be(Stone(1))
    }
    "should handle an invalid input in GPTHREE" in {
      tui.gpTwoSeparator = false
      tui.processGameInputLine("Invalid")

      tui.gpTwoSeparator = true
      tui.processGameInputLine("Invalid")
      tui.gpTwoSeparator = false
    }
    "should handle a valid mill input in GPTHREE" in {
      println(tui.currentPlayer)
      // Player Two Stone move
      tui.processGameInputLine("15")
      tui.processGameInputLine("33")
      // Player One Stone move
      tui.processGameInputLine("11")
      tui.processGameInputLine("32")

      tui.newMill should be(true)
      // Player One Remove Competitor Stone
      tui.processGameInputLine("33")
    }

    "should change from GPTHREE to END" in {
      controller.players(0).MAX_STONE = 2
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.END)

      controller.players(0).MAX_STONE = 9
      controller.players(1).MAX_STONE = 2
      controller.gameStatus = GameStatus.GPTHREE
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.END)
    }

    "should provide a welcome screen" in {
      val welcomeScreen =
        "**********************************************************************************************\n" +
          "*                                       WELCOME TO                                           *\n"   +
          "*  __________   __     __   ________      _____      ______    __    __           __         *\n"   +
          "* |___    ___| |  |   |  | |   _____|    |   _  \\   /  _   |  |  |  |  |         |  |        *\n"  +
          "*     |  |     |  |___|  | |  |_____     |  | \\  \\_/  / |  |  |  |  |  |         |  |        *\n" +
          "*     |  |     |   ___   | |  ______|    |  |  \\_____/  |  |  |  |  |  |         |  |        *\n"  +
          "*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *\n"   +
          "*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *\n"   +
          "*                                        IN SCALA                                            *\n"   +
          "**********************************************************************************************\n"   +
          "Press 'n' for new Game\nPress 'h' for help\nPress 'q' to quit\n"
      tui.welcomeScreen() should be(welcomeScreen)
    }
    "should provide an endscreen" in {
      val endScreen =
        "                         ***************************************\n" +
        "                            Congratulations PlayerOne!\n" +
        "                            you won the game\n" +
        "                            Press q to quit or n for new Game\n" +
        "                         ***************************************"
        tui.endGameScreen(controller.players(0)) should be(endScreen)
    }
    "should provide a goodbye screen" in {
      val goodbyeScreen =
        "**********************************************************************************************\n" +
          "*                                  THANK YOU FOR PLAYING                                     *\n"   +
          "*  __________   __     __   ________      _____      ______    __    __           __         *\n"   +
          "* |___    ___| |  |   |  | |   _____|    |   _  \\   /  _   |  |  |  |  |         |  |        *\n"  +
          "*     |  |     |  |___|  | |  |_____     |  | \\  \\_/  / |  |  |  |  |  |         |  |        *\n" +
          "*     |  |     |   ___   | |  ______|    |  |  \\_____/  |  |  |  |  |  |         |  |        *\n"  +
          "*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *\n"   +
          "*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *\n"   +
          "*                                        IN SCALA                                            *\n"   +
          "**********************************************************************************************\n"
      tui.goodbyeScreen() should be(goodbyeScreen)
    }
    "should provide a help board" in {
      val helpBoard =
        "To access the Nodes see the following coordinates:\n" +
          "               O----------------------------O----------------------------O\n" +
          "               | (11)                       | (12)                  (13) |\n" +
          "               |                            |                            |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |          | (21)            | (22)       (23) |          |\n" +
          "               |          |            (32) |                 |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |         | (31)     (33) |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               O----------O---------O (38)     (34) O---------O----------O\n" +
          "               | (18)     | (28)    |               |    (24) |     (14) |\n" +
          "               |          |         | (37)     (35) |         |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |            (36) |                 |          |\n" +
          "               |          | (27)            | (26)       (25) |          |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |                            |                            |\n" +
          "               | (17)                       | (16)                  (15) |\n" +
          "               O----------------------------O----------------------------O\n"
      tui.helpBoard() should be(helpBoard)
    }
    "should provide a player one name input" in {
      val playerOneInput = "Please enter name of player one: "
      tui.playerOneName() should be(playerOneInput)
    }
    "should provide a player two name input" in {
      val playerTwoInput = "Please enter name of player two: "
      tui.playerTwoName() should be(playerTwoInput)
    }
    "should provide a Game-Phase-One phrase" in {
      val gpOne = "Let the game begin.\nGame Phase One: Please place your stones on a free field."
      tui.gamePhaseOneBegin() should be(gpOne)
    }
    "should provide a Game-Phase-Two phrase" in {
      val gpTwo = "Game Phase Two: Move your Stones strategically and get the victory."
      tui.gamePhaseTwoBegin() should be(gpTwo)
    }
    "should provide a Game-Phase-Three phrase" in {
      val gpThree = "Game Phase Three: Be aware! One Player is able to jump."
      tui.gamePhaseThreeBegin() should be(gpThree)
    }
    "should provide a warning if wrong coordinates were used" in {
      val warningMsg = "Invalid coordinates entered.\nPlease select another free coordinates."
      tui.coordinationWarning() should be(warningMsg)
    }
    "should provide a warning if stone position is already used" in {
      val warningMsg = "Stone location already used.\nPlease select another free coordinates."
      tui.stoneWarning() should be(warningMsg)
    }
    "should provide a message to show which player is playing with the amount of stones played" in {
      controller.create_empty_Board()
      tui.currentPlayer = new Player("Your Name", 1, 9)
      val playerMsg = "\nYour Name it is your turn Place one stone on a specific coordinate (1 of 9):"
      tui.playerGamePhaseOneTurns() should be(playerMsg)
    }
    "should provide a dialog in GPTWO for player interaction" in {
      tui.currentPlayer = controller.players(0)
      tui.gpTwoSeparator = false
      tui.mainGamePhaseTurns() should be("PlayerOne choose the stone you want to move:")
      tui.gpTwoSeparator = true
      tui.mainGamePhaseTurns() should be("PlayerOne where do you want to place it:")
    }
    "should provide the current gamescore (amount of stones)" in {
      controller.players(0).MAX_STONE = 9
      controller.players(1).MAX_STONE = 9
      tui.currentGameScore() should be("                                         9 vs. 9\n")
    }
    "should update the board with new stones" in {
      val emptyBoard = new Board
      val filledPlayerOneBoard = emptyBoard.update_board(0, 0, 1)
      val filledPlayerTwoBoard = emptyBoard.update_board(0, 0, 2)

      val emptyBoardString =
        "               O----------------------------O----------------------------O\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               O----------O---------O               O---------O----------O\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               O----------------------------O----------------------------O\n"
      val filledPlayerOneBoardString =
        "               W----------------------------O----------------------------O\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               O----------O---------O               O---------O----------O\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               O----------------------------O----------------------------O\n"
      val filledPlayerTwoBoardString =
        "               B----------------------------O----------------------------O\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               O----------O---------O               O---------O----------O\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         |               |         |          |\n" +
          "               |          |         O-------O-------O         |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          |                 |                 |          |\n" +
          "               |          O-----------------O-----------------O          |\n" +
          "               |                            |                            |\n" +
          "               |                            |                            |\n" +
          "               O----------------------------O----------------------------O\n"
      tui.updateBoard(emptyBoard) should be(emptyBoardString)
      tui.updateBoard(filledPlayerOneBoard) should be(filledPlayerOneBoardString)
      tui.updateBoard(filledPlayerTwoBoard) should be(filledPlayerTwoBoardString)
    }
  }
}
