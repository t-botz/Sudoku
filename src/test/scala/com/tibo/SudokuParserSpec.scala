package com.tibo

import scala.io.Source

/**
  * Created by tibo.delor on 7/01/2016.
  */

class SudokuParserSpec extends org.specs2.mutable.Specification {
  "Parser" >> {
    "parse a valid board" >> {
      val parsed: Seq[Seq[Option[Int]]] = SudokuParser.parse(Source.fromFile("/Users/tibo.delor/SudokuSolver/src/test/resources/Sudoku1_Basic.psv"))
      val expected: Seq[Seq[Option[Int]]] = Seq(
        Seq(None,None,Some(8),None,None,None,None,None,None),
        Seq(Some(3),None,Some(7),None,Some(4),None,Some(2),None,Some(5)),
        Seq(None,None,None,None,Some(7),None,Some(1),Some(9),None),
        Seq(None,Some(5),Some(6),None,None,Some(7),None,Some(8),Some(9)),
        Seq(None,None,None,Some(3),None,Some(8),None,None,None),
        Seq(Some(8),Some(2),None,Some(9),None,None,Some(6),Some(4),None),
        Seq(None,Some(8),Some(4),None,Some(3),None,None,None,None),
        Seq(Some(2),None,Some(5),None,Some(8),None,Some(4),None,Some(1)),
        Seq(None,None,None,None,None,None,Some(8),None,None)
      )

      parsed must_== expected
    }
  }
}