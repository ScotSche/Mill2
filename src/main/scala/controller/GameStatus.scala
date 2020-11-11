package controller

object GameStatus extends Enumeration {

  type GameStatus = Value
  val IDLE, GPONE, GPTWO , GPTHREE= Value

  val map = Map[GameStatus, String](
    IDLE -> "",
    GPONE -> "Let the game begin.\nGame Phase One: Please place your stones on a free field.",
    GPTWO -> "Game Phase Two: Move your Stones strategically and get the victory.",
    GPTHREE -> "Game Phase Three: Be aware! One Player is able to jump."
  )

  def message(gameStatus: GameStatus) ={
    map(gameStatus)
  }
}