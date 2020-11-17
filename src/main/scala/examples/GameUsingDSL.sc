import com.sun.net.httpserver.Authenticator.Success

import scala.util.{Failure, Success, Try}

case class eitherPattern(input: Try[Any]) {
  def validLenght:eitherPattern = input match {
    case Success(str: String) => if(str.length == 2) copy(Success(str)) else throw new Exception("Invalid lenght")
    case Failure(exception) => copy(Failure(exception))
  }
  def validInt: eitherPattern = input match {
    case Success(str: String) => if(str forall Character.isDigit) copy(Success(str)) else throw new Exception("No Integer")
    case Failure(exception) => copy(Failure(exception))
  }
}
val test = Try(eitherPattern(Success("11")).validLenght.validInt.input).get
