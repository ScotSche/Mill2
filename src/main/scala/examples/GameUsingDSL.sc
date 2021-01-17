
val boardVector = Vector(Vector(1, 0, 0), Vector(0, 1, 0), Vector(0, 0, 0))

def check_Board_For_Neighbours(color: Int): Boolean = {

  val stoneCoordinates = for{
    (i, j) <- boardVector.zipWithIndex
    (s, n) <- i.zipWithIndex
    if s == color
  } yield {
      val coord = (j, n)
      coord
    }
  val stoneNeighbours = for{
    i <- stoneCoordinates
  } yield {
    val neighbourList = Vector((i._1, if(i._2 - 1 < 0) 2 else i._2 - 1),(i._1, if(i._2 + 1 > 2) 0 else i._2 + 1))
    neighbourList
  }
  val neighbours = stoneNeighbours.flatMap(i => i).filter(i => i == 0)
  println(stoneCoordinates)
  println(neighbours)
  if( neighbours.nonEmpty) true else false
}

check_Board_For_Neighbours(1)