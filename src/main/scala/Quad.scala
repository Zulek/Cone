import scala.util.Try

sealed trait Quad {
  val leftUp: Int
  val rightUp: Int
  val leftDown: Int
  val rightDown: Int
  val isEmpty: Boolean

  def prettyString: String = "|" + leftUp + "‾" + rightUp + "|\n|" + leftDown + "_" + rightDown + "|"

  override def toString: String = leftUp + " " + rightUp + " " + leftDown + " " + rightDown
}

object Quad {
  /**
   * Квадрат со значениями
   */
  case class ValueQuad(id: Int, leftUp: Int, rightUp: Int, leftDown: Int, rightDown: Int) extends Quad {
    override val isEmpty: Boolean = false
  }

  /**
   * Пустой квадрат
   */
  case object EmptyQuad extends Quad {
    override val isEmpty: Boolean = true
    val leftUp: Int = 0
    val rightUp: Int = 0
    val leftDown: Int = 0
    val rightDown: Int = 0
  }

  def getSquaresFromStrings(input: List[String]): Try[List[ValueQuad]] = Try {
    input.zipWithIndex
      .map { case (line, id) =>
        line.split(' ').toList.map(_.toInt) match {
          case lu :: ru :: ld :: rd :: Nil =>
            ValueQuad(id = id, leftUp = lu, rightUp = ru, leftDown = ld, rightDown = rd)
        }
      }
  }

  /**
   * 4 квадрата
   */
  case class HigherQuad(quadLeftUp: Quad, quadRightUp: Quad, quadLeftDown: Quad, quadRightDown: Quad) {
    def centerSumIsValid: Boolean =
      if (List(quadLeftUp, quadRightUp, quadLeftDown, quadRightDown).exists(_.isEmpty))
        quadLeftUp.rightDown + quadRightUp.leftDown + quadLeftDown.rightUp + quadRightDown.leftUp <= 10
      else
        quadLeftUp.rightDown + quadRightUp.leftDown + quadLeftDown.rightUp + quadRightDown.leftUp == 10

    def prettyString: String = {
      val leftString = quadLeftUp.prettyString + "\n" + quadLeftDown.prettyString
      val rightString = quadRightUp.prettyString + "\n" + quadRightDown.prettyString
      leftString.split('\n').zip(rightString.split('\n')).map(x => x._1 + x._2).mkString("\n")
    }
  }
}