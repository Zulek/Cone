import Quad.{EmptyQuad, HigherQuad}

case class TwelveSquares(q1: Quad, q2: Quad, q3: Quad, q4: Quad, q5: Quad, q6: Quad, q7: Quad, q8: Quad, q9: Quad, q10: Quad, q11: Quad, q12: Quad) {

  override def toString: String =
    s"""$q1
       |$q2
       |$q3
       |$q4
       |$q5
       |$q6
       |$q7
       |$q8
       |$q9
       |$q10
       |$q11
       |$q12
       |""".stripMargin

  def prettyString: String = {
    quad2String(q1.prettyString, q2.prettyString, "     ") + "\n" +
      quad4String(q3, q4, q5, q6) + "\n" +
      quad4String(q7, q8, q9, q10) + "\n" +
      quad2String(q11.prettyString, q12.prettyString, "     ") + "\n"
  }

  private def quad2String(q1: String, q2: String, str: String = "") = {
    q1.split('\n').zip(q2.split('\n')).map(x => str + x._1 + x._2 + str).mkString("\n")
  }

  private def quad4String(q1: Quad, q2: Quad, q3: Quad, q4: Quad) = {
    quad2String(quad2String(quad2String(q1.prettyString, q2.prettyString), q3.prettyString), q4.prettyString)
  }
}

object TwelveSquares {
  private def get2Quads(quads: List[Quad]) = for {
    a <- quads
    quadsMinusA = quads.filterNot(_ == a)
    b <- quadsMinusA
    quadsMinusAB = quadsMinusA.filterNot(_ == b)
  } yield ((a, b), quadsMinusAB)

  def getPossibleAllQuads(all: List[Quad]): List[TwelveSquares] = for {
    q4 <- all
    minus4 = all.filterNot(_ == q4)
    q5 <- minus4
    minus45 = minus4.filterNot(_ == q5)
    q8 <- minus45
    minus458 = minus45.filterNot(_ == q8)
    q9 <- minus458
    minus4589 = minus458.filterNot(_ == q9)
    if HigherQuad(q4, q5, q8, q9).centerSumIsValid

    ((q1, q2), minus12) <- get2Quads(minus4589)
    if HigherQuad(q1, q2, q4, q5).centerSumIsValid
    if HigherQuad(EmptyQuad, EmptyQuad, q1, q2).centerSumIsValid

    ((q6, q10), minus610) <- get2Quads(minus12)
    if HigherQuad(q5, q6, q9, q10).centerSumIsValid
    if HigherQuad(q6, EmptyQuad, q10, EmptyQuad).centerSumIsValid

    ((q12, q11), minus1211) <- get2Quads(minus610)
    if HigherQuad(q8, q9, q11, q12).centerSumIsValid
    if HigherQuad(q11, q12, EmptyQuad, EmptyQuad).centerSumIsValid

    ((q7, q3), _) <- get2Quads(minus1211)
    if HigherQuad(q3, q4, q7, q8).centerSumIsValid
    if HigherQuad(EmptyQuad, q3, EmptyQuad, q7).centerSumIsValid

    if HigherQuad(EmptyQuad, q1, q3, q4).centerSumIsValid
    if HigherQuad(q2, EmptyQuad, q5, q6).centerSumIsValid
    if HigherQuad(q9, q10, q12, EmptyQuad).centerSumIsValid
    if HigherQuad(q7, q8, EmptyQuad, q11).centerSumIsValid

  } yield TwelveSquares(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12)
}