package com.tibo

import com.tibo.util.FileReader


/**
  * Created by tibo.delor on 7/01/2016.
  */

class SudokuSolverSpec extends org.specs2.mutable.Specification {
  val simpleSudoku = SudokuParser.parse(FileReader.read("/Sudoku1_Basic.psv"))

  def containsAllInitalValues(origin:Sudoku, solved:Sudoku) ={
    for {
      row <- 0 to 8
      col <- 0 to 8
      if origin.cells(row)(col).nonEmpty
    } yield origin.cells(row)(col) must_==  solved.cells(row)(col)
  }

  "Sudoku" >> {
    val area1_1 = Seq(8,7,3)
    val area2_2 = Seq(8,7,3,9)
    val area2_3 = Seq(3, 8)
    simpleSudoku.getAllCellsInArea(0,0).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(0,1).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(0,2).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(1,0).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(1,1).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(1,2).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(2,0).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(2,1).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)
    simpleSudoku.getAllCellsInArea(2,2).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area1_1)

    simpleSudoku.getAllCellsInArea(3,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(3,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(3,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(4,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(4,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(4,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(5,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(5,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)
    simpleSudoku.getAllCellsInArea(5,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_2)

    simpleSudoku.getAllCellsInArea(6,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(6,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(6,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(7,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(7,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(7,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(8,3).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(8,4).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
    simpleSudoku.getAllCellsInArea(8,5).filter(_.nonEmpty).map(_.get) must containTheSameElementsAs(area2_3)
  }
  "Solver" >> {

    "solve simple" >> {
      val Some(solvedSudoku) = SudokuSolver.solve(simpleSudoku)

      containsAllInitalValues(simpleSudoku, solvedSudoku)

      solvedSudoku.isSolved must beTrue

    }
  }
}