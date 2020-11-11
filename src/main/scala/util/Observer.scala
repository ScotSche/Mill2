package util

trait Observer {
  def update: Unit
  def updatePlayer: Unit
}

class Observable {
  var subscribers: Vector[Observer] = Vector()

  def add(s: Observer): Unit = subscribers = subscribers :+ s

  // def remove(s: Observer): Unit = subscribers = subscribers.filterNot(o => o == s)

  def notifyObservers: Unit = subscribers.foreach(o => o.update)
  def notifyPlayerObserver: Unit = subscribers.foreach(o => o.updatePlayer)
}

