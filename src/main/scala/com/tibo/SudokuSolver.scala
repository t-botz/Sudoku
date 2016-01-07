package com.tibo


case class Sudoku(cells: Seq[Seq[Option[Int]]]) {

  def isSolved = nbUnsolved == 0

  def nbUnsolved = cells.flatten.count(_.isEmpty)

  def getAlreadyUsed(row: Int, col: Int): Seq[Int] = {
    {
        cells(row) ++
        cells.map(l => l(col)) ++
        getAllCellsInArea(row, col)
    }.filter(_.nonEmpty).map(_.get).distinct
  }
  def getPotentialValues(row: Int, col: Int): Seq[Int] = {
    1 to 9 diff getAlreadyUsed(row,col)
  }

  def getAllCellsInArea(row: Int, col: Int): Seq[Option[Int]] = {
    val firstRow: Int = row - row % 3
    val firstCol: Int = col - col % 3
    firstRow to firstRow + 2 flatMap { currentRow =>
      firstCol to firstCol + 2 map { currentCol =>
        cells(currentRow)(currentCol)
      }
    }
  }

  override def toString() ={
    "| " + cells.map(_.map {
      case None => "."
      case Some(x) => x.toString
    }.mkString(" | ")).mkString(" |\n| ") + " |"
  }
}

/**
  * Created by tibo.delor on 7/01/2016.
  */
object SudokuSolver {

  def potentialSolutionsByCell(sudoku: Sudoku): Seq[Seq[Seq[Int]]] = {
    sudoku.cells.zipWithIndex.map { case (row, rowIndex) =>
      row.zipWithIndex.map {
        case (Some(value), colIndex) => Seq(value)
        case (value, colIndex) => {
          sudoku.getPotentialValues(rowIndex, colIndex)
        }

      }
    }
  }

  def isValid(potentialSolutions: Seq[Seq[Seq[Int]]]): Boolean = {
    !potentialSolutions.exists(_.exists(_.isEmpty))
  }

  def toSudoku(potentialValues: Seq[Seq[Seq[Int]]]): Sudoku = Sudoku(potentialValues.map(_.map { solutions =>
    if(solutions.size == 1) Some(solutions.head)
    else None
  }))

  def solve(sudoku: Sudoku): Option[Sudoku] = {
    if (sudoku.isSolved)
      Some(sudoku)
    else {
      val sudokuAsPotentialValues: Seq[Seq[Seq[Int]]] = potentialSolutionsByCell(sudoku)
      if (!isValid(sudokuAsPotentialValues)) {
        None
      }
      else {
        val solvedSudoku = toSudoku(sudokuAsPotentialValues)
        if (solvedSudoku.nbUnsolved == sudoku.nbUnsolved)
          solveByGuess(sudoku)
        else
          solve(solvedSudoku)
      }
    }
  }

  def solveByGuess (sudoku: Sudoku) = ???

}
