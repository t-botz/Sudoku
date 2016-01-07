package com.tibo


case class Sudoku(rows: Seq[Seq[Option[Int]]]) {

  lazy val isSolved = nbUnsolved == 0

  lazy val  nbUnsolved = rows.flatten.count(_.isEmpty)

  def getAlreadyUsed(row: Int, col: Int): Seq[Int] = {
    {
      rows(row) ++
      byColumns(col) ++
      getAllCellsInArea(row, col)
    }.filter(_.nonEmpty).map(_.get).distinct
  }

  def hasNoDuplicate(cells:Seq[Option[Int]]):Boolean = {
    val onlyFilledCells: Seq[Option[Int]] = cells.filter(_.nonEmpty)
    onlyFilledCells.size == onlyFilledCells.distinct.size
  }
  lazy val  isValid: Boolean ={
    rows.forall(hasNoDuplicate) &&
    byColumns.forall(hasNoDuplicate) &&
    byAreas.forall(hasNoDuplicate)
  }

  lazy val byColumns = {
    0 to 8 map { col => rows.map(_ (col))}
  }
  lazy val byAreas = {
    0 to 8 map { areaNb => getAllCellsInArea(areaNb , areaNb * 3 % 9)}
  }

  def getPotentialValues(row: Int, col: Int): Seq[Int] = {
    1 to 9 diff getAlreadyUsed(row, col)
  }

  def getAllCellsInArea(row: Int, col: Int): Seq[Option[Int]] = {
    val firstRow: Int = row - row % 3
    val firstCol: Int = col - col % 3
    firstRow to firstRow + 2 flatMap { currentRow =>
      firstCol to firstCol + 2 map { currentCol =>
        rows(currentRow)(currentCol)
      }
    }
  }

  def update(row:Int, col:Int, value: Option[Int]):Sudoku = {
    Sudoku(rows.updated(row, rows(row).updated(col, value)))
  }

  override def toString() = {
    "| " + rows.map(_.map {
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
    sudoku.rows.zipWithIndex.map { case (row, rowIndex) =>
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
    if (solutions.size == 1) Some(solutions.head)
    else None
  }))

  def solve(sudoku: Sudoku): Option[Sudoku] = {
    lazy val sudokuAsPotentialValues: Seq[Seq[Seq[Int]]] = potentialSolutionsByCell(sudoku)

    def solveByGuess: Option[Sudoku] = {
      val (row, col, solutionsExplored) = sudokuAsPotentialValues.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.map { case (solutions, colIndex) =>
          (rowIndex, colIndex, solutions)
        }
      }.filter(_._3.size > 1).sortBy(_._3.size).head

      solutionsExplored.map { guess =>
        solve(sudoku.update(row, col, Some(guess)))
      }.find(_.nonEmpty).flatten
    }


    if (sudoku.isSolved)
      Some(sudoku)
    else if(!sudoku.isValid){
      None
    }
    else {
      if (!isValid(sudokuAsPotentialValues)) {
        None
      }
      else {
        val solvedSudoku = toSudoku(sudokuAsPotentialValues)
        if (solvedSudoku.nbUnsolved == sudoku.nbUnsolved)
          solveByGuess
        else
          solve(solvedSudoku)
      }
    }
  }


}
