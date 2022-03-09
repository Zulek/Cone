import scala.util.Failure

object Boot extends App {
  val file = scala.io.Source.fromFile("input")
  val lines = file.getLines().toList
  file.close()

  val res = for {
    squares <- Quad.getSquaresFromStrings(lines)
    twSquares = TwelveSquares.getPossibleAllQuads(squares)
  } yield twSquares.foreach(println)

  res match {
    case Failure(exception) => println(exception)
      System.exit(1)
    case _ =>
  }
}