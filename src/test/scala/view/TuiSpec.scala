package view

import controller.{Controller, GameStatus}
import model.{Board, Player, Stone}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class TuiSpec extends AnyWordSpec with Matchers {

  "A Mill Tui" should{
    val controller = new Controller(new Board, Vector())
    controller.create_new_Players("PlayerOne", "PlayerTwo")
    val tui = new Tui(controller)

    //  processInputLine Methode
    "create a new board on input 'n'" in {
      tui.processInputLine("n")
      controller.board should be(new Board)
    }
    "should provide a help board on input 'h'" in {
      tui.processInputLine("h")
    }
    "should end the game on input 'q'" in {
      tui.processInputLine("q")
    }
    "should notify user on any other input" in {
      tui.processInputLine("Test")
    }
    //  processGameInputLine Methode
    "should end the game in gameMode on input 'q'" in {
      tui.processGameInputLine("q")
    }
    "should provide a help board in gameMode on input 'h'" in {
      tui.processGameInputLine("h")
    }
    "should start a new game in gameMode on input 'n'" in {
      controller.players(0).MAX_STONE = 8
      controller.players(1).MAX_STONE = 8
      controller.gameStatus = GameStatus.IDLE
      tui.currentPlayer = controller.players(1)
      tui.processGameInputLine("n")

      controller.players(0).MAX_STONE should be(9)
      controller.players(1).MAX_STONE should be(9)
      controller.gameStatus should be(GameStatus.GPONE)
    }

    "should change between both players during the game" in {
      tui.currentPlayer = controller.players(0)
      tui.changePlayer(controller.players) should be(controller.players(1))

      tui.currentPlayer = controller.players(1)
      tui.changePlayer(controller.players) should be(controller.players(0))
    }
    "should provide a welcome screen" in {
      val welcomeScreen: String = """
        :**********************************************************************************************
        :*                                       WELCOME TO                                           *
        :*  __________   __     __   ________      _____      ______    __    __           __         *
        :* |___    ___| |  |   |  | |   _____|    |   _  \   /  _   |  |  |  |  |         |  |        *
        :*     |  |     |  |___|  | |  |_____     |  | \  \_/  / |  |  |  |  |  |         |  |        *
        :*     |  |     |   ___   | |  ______|    |  |  \_____/  |  |  |  |  |  |         |  |        *
        :*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *
        :*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *
        :*                                        IN SCALA                                            *
        :**********************************************************************************************
        :Press 'n' for new Game
        :Press 'h' for help
        :Press 'q' to quit """.stripMargin(':')
      tui.welcomeScreen() should be(welcomeScreen)
    }
    "should provide a goodbye screen" in {
      val goodbyeScreen: String = """
        :**********************************************************************************************
        :*                                  THANK YOU FOR PLAYING                                     *
        :*  __________   __     __   ________      _____      ______    __    __           __         *
        :* |___    ___| |  |   |  | |   _____|    |   _  \   /  _   |  |  |  |  |         |  |        *
        :*     |  |     |  |___|  | |  |_____     |  | \  \_/  / |  |  |  |  |  |         |  |        *
        :*     |  |     |   ___   | |  ______|    |  |  \_____/  |  |  |  |  |  |         |  |        *
        :*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *
        :*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *
        :*                                        IN SCALA                                            *
        :********************************************************************************************** """.stripMargin(':')
    tui.goodbyeScreen() should be(goodbyeScreen)
    }
    "should provide an endgame screen" in {
      val endgameScreen: String = """
        :                         ***************************************
        :                            Congratulations PlayerOne!
        :                            you won the game
        :                            Press q to quit or n for new Game
        :                         *************************************** """.stripMargin(':')
      tui.endGameScreen(controller.players(0)) should be(endgameScreen)
    }
    "should provide a player one input string" in {
      val inputString: String = """
        |Please enter name of player one: """.stripMargin
      tui.playerOneName() should be(inputString)
    }
    "should provide a player two input string" in {
      val inputString: String = """
        |Please enter name of player two: """.stripMargin
      tui.playerTwoName() should be(inputString)
    }
    "should start Game phase one" in {
      val gpOneString: String = "Let the game begin.\nGame Phase One: Please place your stones on a free field."
      tui.gamePhaseOneBegin() should be(gpOneString)
      controller.gameStatus should be(GameStatus.GPONE)
    }
    "should start Game phase two" in {
      val gpTwoString: String = "Game Phase Two: Move your Stones strategically and get the victory."
      tui.gamePhaseTwoBegin() should be(gpTwoString)
      controller.gameStatus should be(GameStatus.GPTWO)
    }
    "should start Game phase three" in {
      val gpThreeString: String = "Game Phase Three: Be aware! One Player is able to jump."
      tui.gamePhaseThreeBegin() should be(gpThreeString)
      controller.gameStatus should be(GameStatus.GPTHREE)
    }
    "should provide information to each player of amount of curent stones and total stones" in {
      val informationString: String = """
       |PlayerOne it is your turn Place one stone on a specific coordinate (1 of 9):""".stripMargin('|')
      tui.currentPlayer = controller.players(0)
      tui.playerGamePhaseOneTurns() should be(informationString)
    }
    "should show specific information within main phase to position stone" in {
      tui.gpTwoSeparator = false
      val firstString: String = "PlayerOne choose the stone you want to move:"
      tui.mainGamePhaseTurns() should be(firstString)

      tui.gpTwoSeparator = true
      val secondString: String = "PlayerOne where do you want to place it:"
      tui.mainGamePhaseTurns() should be(secondString)
    }
    "should provide current game score" in {
      val gameScoreString: String = "                                         9 vs. 9\n"
      tui.currentGameScore() should be(gameScoreString)
    }
    "should match color from integer to readable Strings (chars)" in {
      tui.color_matcher(Stone(0)) should be("O")
      tui.color_matcher(Stone(1)) should be("W")
      tui.color_matcher(Stone(2)) should be("B")
    }
    "should show dynamic board dependent on stones" in {
      val boardString: String = """
        :               O----------------------------O----------------------------O
        :               |                            |                            |
        :               |                            |                            |
        :               |          O-----------------O-----------------O          |
        :               |          |                 |                 |          |
        :               |          |                 |                 |          |
        :               |          |         O-------O-------O         |          |
        :               |          |         |               |         |          |
        :               |          |         |               |         |          |
        :               O----------O---------O               O---------O----------O
        :               |          |         |               |         |          |
        :               |          |         |               |         |          |
        :               |          |         O-------O-------O         |          |
        :               |          |                 |                 |          |
        :               |          |                 |                 |          |
        :               |          O-----------------O-----------------O          |
        :               |                            |                            |
        :               |                            |                            |
        :               O----------------------------O----------------------------O""".stripMargin(':')
      tui.updateBoard(controller.board) should be(boardString)
    }
    "should do nothing to updatePlayer on Gamestatus END" in {
      controller.gameStatus = GameStatus.END
      tui.updatePlayer()
    }
    "should handle an invalid select stone input" in {
      tui.currentPlayer = controller.players(0)
      tui.handleNormalSelectStone("11")
      tui.gpTwoList.isEmpty should be(true)
    }
  }
}
/*"A Mill Tui" should{
    val controller = new Controller(new Board, Vector())
    controller.create_new_Players("PlayerOne", "PlayerTwo")
    val tui = new Tui(controller)
    "create a new board on input 'n'" in{
      tui.processInputLine("n")
      controller.board should be(new Board)
    }
    "should start with player one" in {
      tui.update()
      tui.currentPlayer should be(controller.players(1))
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
    "should start a new game on input 'n' in game mode" in {
      tui.processGameInputLine("n")
      controller.players(0).MAX_STONE should be(9)
      controller.players(1).MAX_STONE should be(9)
      tui.currentPlayer should be(controller.players(1))
      controller.board should be(new Board)
    }

    "should handle a valid input in GPONE" in {
      controller.gameStatus = GameStatus.GPONE
      controller.create_empty_Board()

      // PlayerOne
      tui.processGameInputLine("11")
      controller.board.stone(0, 0) should be(Stone(1))

      // PlayerTwo
      tui.processGameInputLine("12")
      controller.board.stone(0, 1) should be(Stone(2))
    }
    "should handle an invalid input in GPONE" in {
      // PlayerOne
      tui.processGameInputLine("Invalid")
    }
    "should handle an invalid and a valid mill input in GPONE" in {
      // PlayerOne
      tui.processGameInputLine("18")
      // PlayerTwo
      tui.processGameInputLine("13")
      // PlayerOne
      tui.processGameInputLine("17")
      tui.newMill should be(true)
      // PlayerOne Invalid
      tui.processGameInputLine("00")
      // PlayerOne Valid
      tui.processGameInputLine("12")
      controller.board.check_stone_Set(0, 1) should be(false)
    }
    "should change from GPONE to GPTWO" in {
      // PlayerTwo
      tui.processGameInputLine("12")
      // PlayerOne
      tui.processGameInputLine("14")
      // PlayerTwo
      tui.processGameInputLine("15")
      // PlayerOne
      tui.processGameInputLine("16")
      // PlayerTwo
      tui.processGameInputLine("21")
      // PlayerOne
      tui.processGameInputLine("22")
      // PlayerTwo
      tui.processGameInputLine("23")
      // PlayerOne
      tui.processGameInputLine("24")
      // PlayerTwo
      tui.processGameInputLine("25")
      // PlayerOne
      tui.processGameInputLine("26")
      // PlayerTwo
      tui.processGameInputLine("27")
      // PlayerOne
      tui.processGameInputLine("28")
      // PlayerTwo
      tui.processGameInputLine("31")
      controller.gameStatus should be(GameStatus.GPTWO)
    }

    "should handle a valid input in GPTWO" in {
      // PlayerOne
      tui.processGameInputLine("28")
      tui.processGameInputLine("38")

      controller.board.stone(1, 7) should be(Stone(0))
      controller.board.stone(2, 7) should be(Stone(1))
    }
    "should handle an invalid input in GPTWO" in {
      tui.gpTwoSeparator = false
      // PlayerTwo
      tui.processGameInputLine("Invalid")

      tui.gpTwoSeparator = true
      // PlayerTwo
      tui.processGameInputLine("Invalid")
      tui.gpTwoSeparator = false
    }
    "should change from GPTWO to GPTHREE" in {
/*      controller.create_empty_Board()
      controller.gameStatus = GameStatus.GPTWO
      controller.players(0).MAX_STONE = 3;
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.GPTHREE)

      controller.players(0).MAX_STONE = 9;
      controller.players(1).MAX_STONE = 3;
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.GPTHREE)*/
    }

    "should change from GPTHREE to END" in {
/*      controller.players(0).MAX_STONE = 2
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.END)

      controller.players(0).MAX_STONE = 9
      controller.players(1).MAX_STONE = 2
      controller.gameStatus = GameStatus.GPTHREE
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.END)

      controller.gameStatus = GameStatus.GPTHREE
      controller.board.update_board(0, 0, 1)
      controller.board.update_board(0, 1, 2)
      controller.board.update_board(0, 7, 2)
      controller.notifyPlayerObserver
      controller.gameStatus should be(GameStatus.END)*/
    }

    "should provide a welcome screen" in {
      val welcomeScreen = """
        :**********************************************************************************************
        :*                                       WELCOME TO                                           *
        :*  __________   __     __   ________      _____      ______    __    __           __         *
        :* |___    ___| |  |   |  | |   _____|    |   _  \   /  _   |  |  |  |  |         |  |        *
        :*     |  |     |  |___|  | |  |_____     |  | \  \_/  / |  |  |  |  |  |         |  |        *
        :*     |  |     |   ___   | |  ______|    |  |  \_____/  |  |  |  |  |  |         |  |        *
        :*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *
        :*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *
        :*                                        IN SCALA                                            *
        :**********************************************************************************************
        :Press 'n' for new Game
        :Press 'h' for help
        :Press 'q' to quit """.stripMargin(':')
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
      val goodbyeScreen = """
        :**********************************************************************************************
        :*                                  THANK YOU FOR PLAYING                                     *
        :*  __________   __     __   ________      _____      ______    __    __           __         *
        :* |___    ___| |  |   |  | |   _____|    |   _  \   /  _   |  |  |  |  |         |  |        *
        :*     |  |     |  |___|  | |  |_____     |  | \  \_/  / |  |  |  |  |  |         |  |        *
        :*     |  |     |   ___   | |  ______|    |  |  \_____/  |  |  |  |  |  |         |  |        *
        :*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *
        :*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *
        :*                                        IN SCALA                                            *
        :********************************************************************************************** """.stripMargin(':')
      tui.goodbyeScreen() should be(goodbyeScreen)
    }
    "should provide a help board" in {
      val helpBoard = """
        :To access the Nodes see the following coordinates:
        :               O----------------------------O----------------------------O
        :               | (11)                       | (12)                  (13) |
        :               |                            |                            |
        :               |          O-----------------O-----------------O          |
        :               |          | (21)            | (22)       (23) |          |
        :               |          |            (32) |                 |          |
        :               |          |         O-------O-------O         |          |
        :               |          |         | (31)     (33) |         |          |
        :               |          |         |               |         |          |
        :               O----------O---------O (38)     (34) O---------O----------O
        :               | (18)     | (28)    |               |    (24) |     (14) |
        :               |          |         | (37)     (35) |         |          |
        :               |          |         O-------O-------O         |          |
        :               |          |            (36) |                 |          |
        :               |          | (27)            | (26)       (25) |          |
        :               |          O-----------------O-----------------O          |
        :               |                            |                            |
        :               | (17)                       | (16)                  (15) |
        :               O----------------------------O----------------------------O """.stripMargin(':')
      tui.helpBoard() should be(helpBoard)
    }
    "should provide a player one name input" in {
      val playerOneInput =
        """
          |Please enter name of player one: """.stripMargin
      tui.playerOneName() should be(playerOneInput)
    }
    "should provide a player two name input" in {
      val playerTwoInput = """
        |Please enter name of player two: """.stripMargin
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
    "should provide a message to show which player is playing with the amount of stones played" in {
      controller.create_empty_Board()
      tui.currentPlayer = Player("Your Name", 1, 9)
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

    "should check in Game phase two if one player has no more moves available" in {
      controller.gameStatus = GameStatus.GPTWO
      controller.create_empty_Board()
      controller.setStone(0, 0, 1)
      controller.setStone(0, 1, 2)
      controller.setStone(0, 7, 2)
      tui.updatePlayer()
      controller.gameStatus should be(GameStatus.END)
    }

    "should do nothing in Game phase END" in {
      controller.gameStatus = GameStatus.END
      tui.updatePlayer()
    }
  }
}
*/
