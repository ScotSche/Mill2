package view

import controller.{Controller, GameStatus}
import model.{Board, MaybeInput, Player, Stone}
import util.Observer

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

// Trigger Travis CI

class Tui(controller: Controller) extends Observer{
  controller.add(this)
  var currentPlayer = Player("", 0, 0)
  var gpTwoSeparator = false
  var gpTwoList = new ListBuffer[(Int, Int)]()

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
      case _ => players(0)
    }
  }

  def processGameInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "h" => println(helpBoard())
      case _ =>
        controller.gameStatus match {
          case GameStatus.GPONE =>
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
                case _ =>
              }
            }
            else println("Invalid")

          case GameStatus.GPTWO =>
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
                    println(playerGamePhaseTwoTurns())
                  case _ =>
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
                  case _ =>
                }
              }
              else println("Invalid")
            }

          case GameStatus.GPTHREE =>
        }
    }
  }
  def welcomeScreen(): String ={
    val welcomeString = "**********************************************************************************************\n" +
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
    welcomeString
  }

  def playerOneName(): String ={
    val playerString = "Please enter name of player one: "
    playerString
  }
  def playerTwoName(): String ={
    val playerString = "Please enter name of player two: "
    playerString
  }

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

  def stoneWarning(): String = {
    val warningString = "Stone location already used.\nPlease select another free coordinates."
    warningString
  }
  def coordinationWarning(): String = {
    val warningString = "Invalid coordinates entered.\nPlease select another free coordinates."
    warningString
  }

  def playerGamePhaseOneTurns(): String ={
    val playerTurnString = s"\n${currentPlayer.name} it is your turn Place one stone on a specific coordinate " + "" +
      s"(${controller.amountOfPlayerStones(currentPlayer.color) + 1} of ${currentPlayer.MAX_STONE}):"
    playerTurnString
  }
  def playerGamePhaseTwoTurns(): String ={
    if( !gpTwoSeparator)
      s"${currentPlayer.name} choose the stone you want to move:"
    else
      s"${currentPlayer.name} where do you want to place it:"
  }
  def playerGamePhaseThreeTurns(): String ={
    val str = ""

    //s"\n${currentPlayer.name} where do you want to place it:"
    //s"\n${currentPlayer.name} choose the stone you want to move:"
    str
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
  def helpBoard(): String ={
    val helpString = "To access the Nodes see the following coordinates:\n" +
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
    helpString
  }

  def goodbyeScreen(): String ={
    val goodbyeString = "**********************************************************************************************\n" +
      "*                                  THANK YOU FOR PLAYING                                     *\n"   +
      "*  __________   __     __   ________      _____      ______    __    __           __         *\n"   +
      "* |___    ___| |  |   |  | |   _____|    |   _  \\   /  _   |  |  |  |  |         |  |        *\n"  +
      "*     |  |     |  |___|  | |  |_____     |  | \\  \\_/  / |  |  |  |  |  |         |  |        *\n" +
      "*     |  |     |   ___   | |  ______|    |  |  \\_____/  |  |  |  |  |  |         |  |        *\n"  +
      "*     |  |     |  |   |  | |  |_____     |  |           |  |  |  |  |  |______   |  |______  *\n"   +
      "*     |__|     |__|   |__| |________|    |__|           |__|  |__|  |_________|  |_________| *\n"   +
      "*                                        IN SCALA                                            *\n"   +
      "**********************************************************************************************\n"
    goodbyeString
  }
  override def update: Unit = {
    if(controller.gameStatus == GameStatus.GPTWO) println(currentGameScore())
    println(updateBoard(controller.board))
    if (currentPlayer.color == 0) {
      currentPlayer = controller.players(0)
      println(playerGamePhaseOneTurns())
    }
    if (controller.newMill) {
      println(s"New Mill on Board\n${currentPlayer.name} what stone do you want to remove?")
      val input = readLine()
      val result = MaybeInput(Some(input)).validLength.validInt.validCoordinates.input
      if (result.isDefined) {
        result match {
          case Some(data: List[Int]) => println(controller.remove_stone(data(0) - 1, data(1) - 1, currentPlayer.color))
        }
      }
      else println("Invalid")
    }
  }

  override def updatePlayer: Unit = {
    currentPlayer = changePlayer(controller.players)
    controller.gameStatus match {
      case GameStatus.GPONE =>
        if (controller.amountOfPlayerStones(1) == controller.players(0).MAX_STONE &&
          controller.amountOfPlayerStones(2) == controller.players(1).MAX_STONE) {
          println(gamePhaseTwoBegin())
          println(playerGamePhaseTwoTurns())
        }
        else println(playerGamePhaseOneTurns())

      case GameStatus.GPTWO =>
        if(controller.players(0).MAX_STONE == 3 || controller.players(1).MAX_STONE == 3) {
          println(gamePhaseThreeBegin())
          println(playerGamePhaseThreeTurns())
        } else println(playerGamePhaseTwoTurns())

      case GameStatus.GPTHREE =>
      case _ =>
    }
  }
}
