package dlx.transform

import org.scalatest.{FlatSpec, Matchers}

sealed trait SudokuSparseMatrix {
  val grid = new Array[Array[Byte]](9)
  for (i <- grid.indices) {
    grid(i) = Array.fill[Byte](9)(0)
  }

  val sparseMatrix: Array[Array[Boolean]] = SudokuProblem.convert(grid)
}

final class SudokuProblemSpec extends FlatSpec with Matchers with SudokuSparseMatrix {
  def getRowIndex(index: Int): Int = index / 81
  def getColIndex(index: Int): Int = (index % 81) / 9
  def getBoxColIndex(colIndex: Int): Int = colIndex / 3 * 9
  def getBoxRowIndex(rowIndex: Int): Int = rowIndex / 3 * 27
  def getBoxIndex(i: Int, j: Int): Int = getBoxRowIndex(i) + getBoxColIndex(j)
  def getCelIndex(index: Int): Int = index % 9

  "celIndex" should "be contained" in {
    SudokuProblem.celColumnIndexBy(0) should be(0)
    SudokuProblem.celColumnIndexBy(8) should be(0)
    SudokuProblem.celColumnIndexBy(9) should be(1)
    SudokuProblem.celColumnIndexBy(729) should be(81)
  }

  "rowIndex" should "be contained" in {
    SudokuProblem.rowColumnIndexBy(0) should be(81)
    SudokuProblem.rowColumnIndexBy(729) should be(162)
  }

  "colIndex" should "be contained" in {
    SudokuProblem.colColumnIndexBy(0) should be(162)
    SudokuProblem.colColumnIndexBy(80) should be(242)
    SudokuProblem.colColumnIndexBy(81) should be(162)
    SudokuProblem.colColumnIndexBy(728) should be(242)
  }

  "boxIndex" should "be contained" in {
    SudokuProblem.boxColumnIndexBy(0) should be(243)
    SudokuProblem.boxColumnIndexBy(9) should be(243)
    SudokuProblem.boxColumnIndexBy(18) should be(243)

    SudokuProblem.boxColumnIndexBy(27) should be(252)
    SudokuProblem.boxColumnIndexBy(36) should be(252)

    SudokuProblem.boxColumnIndexBy(54) should be(261)
    SudokuProblem.boxColumnIndexBy(63) should be(261)

    // Row 2
    SudokuProblem.boxColumnIndexBy(81) should be(243)
    // Row 3
    SudokuProblem.boxColumnIndexBy(162) should be(243)
    // Row 4
    SudokuProblem.boxColumnIndexBy(243) should be(270)
    SudokuProblem.boxColumnIndexBy(324) should be(270)

    SudokuProblem.boxColumnIndexBy(729) should be(324)
  }

  "An empty Sudoku problem" should "be converted correctly" in {
    sparseMatrix.length should be(729)
    for(i <- sparseMatrix.indices) {
      sparseMatrix(i).length should be(324)
      //cells
      val cel = getRowIndex(i) * 9 + getColIndex(i)
      cel should be >= 0
      cel should be < 81

      for (j <- 0 until 81 ) {
        sparseMatrix(i)(j) should be(j == cel)
      }

      // rows
      val row = 81 + getRowIndex(i) * 9 + getCelIndex(i)
      row should be >= 81
      row should be < 162

      for(j <- 81 until 162) {
        sparseMatrix(i)(j) should be(j == row)
      }

      // cols
      val col = 162 + getColIndex(i) * 9  + getCelIndex(i)
      col should be >= 162
      col should be < 243

      for(j <- 162 until 243) {
        sparseMatrix(i)(j) should be (j == col)
      }

      // boxes
      val box = 243 + getBoxIndex(getRowIndex(i), getColIndex(i)) + getCelIndex(i)
      box should be >= 243
      box should be < 324

      for(j <- 243 until 324) {
        sparseMatrix(i)(j) should be (j == box)
      }
    }
  }

  "An empty converted Sudoku problem" should "contains 4 ones in each row" in {
    for(i <- sparseMatrix.indices) {
      sparseMatrix(i).foldLeft(0)((acc, x) => if (x) acc + 1 else acc) should be(4)
    }
  }

  "An empty converted Sudoku problem" should "contains 1 ones in each column" in {
      for(j <- sparseMatrix(0).indices) {
        var acc = 0
        for(i <- sparseMatrix.indices) {
            if(sparseMatrix(i)(j)) acc += 1
        }
        acc should be(9)
    }
  }
}
