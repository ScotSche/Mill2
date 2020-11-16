package view

import controller.{Controller, GameStatus}
import model.{Board, MaybeInput, Player, Stone}
import util.Observer
import scala.collection.mutable.ListBuffer

class Tui(controller: Controller) extends Observer{
  controller.add(this)
  var currentPlayer = Player("", 0, 0)
  var gpTwoSeparator = false
  var gpTwoList = new ListBuffer[(Int, Int)]()
  var newMill = false

  def processInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "h" => println(helpBoard())
      case "n" =>
      case _ => println("No valid input. Please try again!")
    }
  }

  def changePlayer(players: Vector[Player]): Player ={
    currentPlayer.color match {
      case 1 => players(1)
      case 2 => players(0)
    }
  }

  def processGameInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "h" => println(helpBoard())
      case _ =>
        controller.gameStatus match {
          case GameStatus.GPONE =>
            if (!newMill) {
              val verifiedInput = MaybeInput(Some(input))
                .validLength
                .validInt
                .validCoordinates
                .validateStone(controller.board)
                .input

              if (verifiedInput.isDefined) {
                verifiedInput match {
                  case Some(data: List[Int]) =>
                    controller.setStone((data.head - 1), (data(1) - 1), currentPlayer.color)
                }
              }
              else println("Invalid")
            }
            else{ handleMillInputString(input) }

          case GameStatus.GPTWO =>
            if (!newMill) {
              if (!gpTwoSeparator) {
                val verifiedInput = MaybeInput(Some(input))
                  .validLength
                  .validInt
                  .validCoordinates
                  .checkStone(controller.board, currentPlayer.color)
                  .input
                if (verifiedInput.isDefined) {
                  verifiedInput match {
                    case Some(data: List[Int]) =>
                      gpTwoList += Tuple2(data(0) - 1, data(1) - 1)
                      gpTwoSeparator = !gpTwoSeparator
                      println(mainGamePhaseTurns())
                  }
                }
                else println("Invalid")
              }
              else {
                val verifiedInput = MaybeInput(Some(input))
                  .validLength
                  .validInt
                  .validCoordinates
                  .validateStone(controller.board)
                  .input
                if (verifiedInput.isDefined) {
                  verifiedInput match {
                    case Some(data: List[Int]) =>
                      gpTwoList += Tuple2(data(0) - 1, data(1) - 1)
                      val list = gpTwoList.toList
                      controller.moveStone(list(0), list(1), currentPlayer.color)
                      gpTwoList = new ListBuffer[(Int, Int)]
                      gpTwoSeparator = !gpTwoSeparator
                  }
                }
                else println("Invalid")
              }
            }
            else{ handleMillInputString(input) }

          case GameStatus.GPTHREE =>
            if (!newMill) {
              if (!gpTwoSeparator) {
                val verifiedInput = MaybeInput(Some(input))
                  .validLength
                  .validInt
                  .validCoordinates
                  .checkStone(controller.board, currentPlayer.color)
                  .input
                if (verifiedInput.isDefined) {
                  verifiedInput match {
                    case Some(data: List[Int]) =>
                      gpTwoList += Tuple2(data(0) - 1, data(1) - 1)
                      gpTwoSeparator = !gpTwoSeparator
                      println(mainGamePhaseTurns())
                  }
                }
                else println("Invalid")
              }
              else {
                val verifiedInput = MaybeInput(Some(input))
                  .validLength
                  .validInt
                  .validCoordinates
                  .validateStone(controller.board)
                  .input
                if (verifiedInput.isDefined) {
                  verifiedInput match {
                    case Some(data: List[Int]) =>
                      gpTwoList += Tuple2(data(0) - 1, data(1) - 1)
                      val list = gpTwoList.toList
                      controller.moveStone(list(0), list(1), currentPlayer.color)
                      gpTwoList = new ListBuffer[(Int, Int)]
                      gpTwoSeparator = !gpTwoSeparator
                  }
                }
                else println("Invalid")
              }
            }
            else{ handleMillInputString(input) }
        }
    }
  }
  def handleMillInputString(input: String) = {
    val verifiedInput = MaybeInput(Some(input))
      .validLength
      .validInt
      .validCoordinates
      .checkStone(controller.board, {
        val optionPlayer = controller.players.find(_ != currentPlayer)
        optionPlayer match {
          case Some(player: Player) => player.color
        }
      })
      .input

    if (verifiedInput.isDefined) {
      verifiedInput match {
        case Some(data: List[Int]) =>
          newMill = !newMill
          controller.remove_stone((data.head - 1), (data(1) - 1), currentPlayer.color)
      }
    }
    else println("Invalid")
  }

  def welcomeScreen(): String = """
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

  def playerOneName(): String = """
    |Please enter name of player one: """.stripMargin
  def playerTwoName(): String = """
    |Please enter name of player two: """.stripMargin

  def gamePhaseOneBegin(): String = {
    controller.gameStatus = GameStatus.GPONE

    val gpOneString = GameStatus.message(controller.gameStatus)
    gpOneString
  }
  def gamePhaseTwoBegin(): String = {
    controller.gameStatus = GameStatus.GPTWO
    val gpTwoString = GameStatus.message(controller.gameStatus)
    gpTwoString
  }
  def gamePhaseThreeBegin(): String = {
    controller.gameStatus = GameStatus.GPTHREE
    val gpThreeString = GameStatus.message(controller.gameStatus)
    gpThreeString
  }

  def stoneWarning(): String = """
    Stone location already used.
    Please select another free coordinates. """

  def coordinationWarning(): String = """
    Invalid coordinates entered.
    Please select another free coordinates. """

  def playerGamePhaseOneTurns(): String ={
    val playerTurnString = s"\n${currentPlayer.name} it is your turn Place one stone on a specific coordinate " + "" +
      s"(${controller.amountOfPlayerStones(currentPlayer.color) + 1} of ${currentPlayer.MAX_STONE}):"
    playerTurnString
  }
  def mainGamePhaseTurns(): String ={
    if( !gpTwoSeparator)
      s"${currentPlayer.name} choose the stone you want to move:"
    else
      s"${currentPlayer.name} where do you want to place it:"
  }

  def currentGameScore(): String ={
    val gameScore = s"                                         " +
      s"${controller.players(0).MAX_STONE} vs. ${controller.players(1).MAX_STONE}\n"
    gameScore
  }

  def color_matcher(in:Stone):String = {
    in.color match {
      case 0 => "O"
      case 1 => "W"
      case 2 => "B"
    }
  }

  def updateBoard(board: Board): String={
    val uiBoard = board.stones.rows.map(i => i.map(color_matcher))

    val updateString = s"               ${uiBoard(0)(0)}----------------------------${uiBoard(0)(1)}----------------------------${uiBoard(0)(2)}\n" +
      "               |                            |                            |\n" +
      "               |                            |                            |\n" +
      s"               |          ${uiBoard(1)(0)}-----------------${uiBoard(1)(1)}-----------------${uiBoard(1)(2)}          |\n" +
      "               |          |                 |                 |          |\n" +
      "               |          |                 |                 |          |\n" +
      s"               |          |         ${uiBoard(2)(0)}-------${uiBoard(2)(1)}-------${uiBoard(2)(2)}         |          |\n" +
      "               |          |         |               |         |          |\n" +
      "               |          |         |               |         |          |\n" +
      s"               ${uiBoard(0)(7)}----------${uiBoard(1)(7)}---------${uiBoard(2)(7)}               ${uiBoard(2)(3)}---------${uiBoard(1)(3)}----------${uiBoard(0)(3)}\n" +
      "               |          |         |               |         |          |\n" +
      "               |          |         |               |         |          |\n" +
      s"               |          |         ${uiBoard(2)(6)}-------${uiBoard(2)(5)}-------${uiBoard(2)(4)}         |          |\n" +
      "               |          |                 |                 |          |\n" +
      "               |          |                 |                 |          |\n" +
      s"               |          ${uiBoard(1)(6)}-----------------${uiBoard(1)(5)}-----------------${uiBoard(1)(4)}          |\n" +
      "               |                            |                            |\n" +
      "               |                            |                            |\n" +
      s"               ${uiBoard(0)(6)}----------------------------${uiBoard(0)(5)}----------------------------${uiBoard(0)(4)}\n"
    updateString
  }
  def helpBoard(): String = """
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

  def endGameScreen(player: Player): String = {
    controller.gameStatus = GameStatus.END
    val endString = {
       "                         ***************************************\n" +
      s"                            Congratulations ${player.name}!\n" +
       "                            you won the game\n" +
       "                            Press q to quit or n for new Game\n" +
       "                         ***************************************"
    }

    endString
  }

  def goodbyeScreen(): String = """
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

  override def update: Unit = {
    if(controller.gameStatus == GameStatus.GPTWO) println(currentGameScore())
    println(updateBoard(controller.board))
    if (currentPlayer.color == 0) {
      currentPlayer = controller.players(0)
      println(playerGamePhaseOneTurns())
    }
    if (controller.newMill) {
      println(s"New Mill on Board\n${currentPlayer.name} what stone do you want to remove?")
      newMill = !newMill
    }
  }

  override def updatePlayer: Unit = {
    if ( !newMill) {
      currentPlayer = changePlayer(controller.players)
      controller.gameStatus match {
        case GameStatus.GPONE =>
          if (controller.amountOfPlayerStones(1) == controller.players(0).MAX_STONE &&
            controller.amountOfPlayerStones(2) == controller.players(1).MAX_STONE) {
            println(gamePhaseTwoBegin())
            println(mainGamePhaseTurns())
          }
          else println(playerGamePhaseOneTurns())

        case GameStatus.GPTWO =>
          if (controller.players(0).MAX_STONE == 3 || controller.players(1).MAX_STONE == 3) {
            println(gamePhaseThreeBegin())
            println(mainGamePhaseTurns())
          } else println(mainGamePhaseTurns())

        case GameStatus.GPTHREE =>
          if (controller.players(0).MAX_STONE == 2) {
            println(endGameScreen(controller.players(1)))
          }
          else if (controller.players(1).MAX_STONE == 2) {
            println(endGameScreen(controller.players(0)))
          }
      }
    }
  }
}
