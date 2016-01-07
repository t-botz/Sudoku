package com.tibo



case class Sudoku(cells:Seq[Seq[Option[Int]]]){

  def isSolved = nbUnsolved == 0

  def nbUnsolved = cells.flatten.count(_.isEmpty)

  def getPotentialValues(row:Int, col:Int):Seq[Int] = {
    cells(row).filter(_.isEmpty).map(_.get) ++
    cells.map(_(col)).filter(_.isEmpty).map(_.get) ++
    ???


  }

  def getUniqueValueOrNone(row:Int, col:Int) : Option[Int] = {
    getPotentialValues(row,col) match {
      case x :: Nil => Some(x)
      case x :: tail => None
    }
  }

}
/**
  * Created by tibo.delor on 7/01/2016.
  */
object SudokuSolver {

  def potentialSolutionsByCell(sudoku:Sudoku) :Seq[Seq[Seq[Int]]] = {
    sudoku.cells.zipWithIndex.map{ case (row,rowIndex) =>
      row.zipWithIndex.map {
        case (Some(value), colIndex) => Seq(value)
        case (value, colIndex) => {
          sudoku.getPotentialValues(rowIndex, colIndex)
        }

      }
  }
  def toSudoku(potentialValues:Seq[Seq[Seq[Int]]]) : Sudoku = Sudoku(potentialValues.map(_.))

  def solve(sudoku:Sudoku):Option[Sudoku] = {
    if(sudoku.isSolved)
      Some(sudoku)
    else {
      val sudokuAsPotentialValues: Seq[Seq[Seq[Int]]] = potentialSolutionsByCell(sudoku)
      if(sudokuAsPotentialValues.exists(_.exists(_.isEmpty))){
        None
      }
      else if(sudokuAsPotentialValues.map(_.map()))
      }


      }

      val sudokuAfterSolveLoop:Sudoku = Sudoku(
        sudoku.cells.zipWithIndex.map{ case (row,rowIndex) =>
          row.zipWithIndex.map {
            case (value, colIndex) if value.nonEmpty => value
            case (value, colIndex) => {
              sudoku.getUniqueValueOrNone(rowIndex, colIndex)
            }

          }
      })

      if(sudoku.nbUnsolved == sudokuAfterSolveLoop.nbUnsolved)
        solveByGuess(sudoku)
  }


  def solveByGuess(sudoku:Sudoku) = ???

}
