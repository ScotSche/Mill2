import controller.Controller
import model.Board
import view.Tui
import scala.io.StdIn.readLine

// Trigger Travis CI

object Mill {

  //val color = ("black", "white")

  val controller = new Controller(new Board, Vector())
  val tui = new Tui(controller)

  def main(args: Array[String]) = {

    var input: String = ""
    println(tui.welcomeScreen())

    do{
      input = readLine()
      tui.processInputLine(input)
      if(input == "n"){
        playerInputIteration()
        tui.gamePhaseOneBegin()
        controller.create_empty_Board()
        do{
          input = readLine()
          tui.processGameInputLine(input)
        } while (input != "q") // || Spielende
      }
    } while(input != "q")

    println(tui.goodbyeScreen())
  }
  def playerInputIteration(): Unit ={
    print(tui.playerOneName())
    val playerOneName = readLine()
    print(tui.playerTwoName())
    val playerTwoName = readLine()
    controller.create_new_Players(playerOneName, playerTwoName)
  }
}
