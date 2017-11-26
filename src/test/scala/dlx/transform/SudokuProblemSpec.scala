package dlx.transform

import org.scalatest.{FlatSpec, Matchers}

final class SudokuProblemSpec extends FlatSpec with Matchers {
  "An empty Sudoku problem" should "be converted correctly" in {
    val grid = new Array[Array[Byte]](9)
    for (i <- grid.indices) {
      grid(i) = Array.fill[Byte](9)(0)
    }

    val array = SudokuProblem.convert(grid)
    array.length should be(729)
    for(i <- array.indices) {
      array(i).length should be(324)
      //cells
      val cel = SudokuProblem.getRowIndex(i) * 9 + SudokuProblem.getColIndex(i)
      cel should be >= 0
      cel should be < 81

      for (j <- 0 until 81 ) {
        array(i)(j) should be(j == cel)
      }

      // rows
      val row = 81 + SudokuProblem.getRowIndex(i) * 9 + SudokuProblem.getCelIndex(i)
      row should be >= 81
      row should be < 162

      for(j <- 81 until 162) {
        array(i)(j) should be(j == row)
      }
      // cols
      val col = 162 + SudokuProblem.getColIndex(i) * 9 + SudokuProblem.getCelIndex(i)
      col should be >= 162
      col should be < 243

      for(j <- 162 until 243) {
        array(i)(j) should be (j == col)
      }

      // boxes
      val box = 243 +
        SudokuProblem.getBoxIndex(SudokuProblem.getRowIndex(i), SudokuProblem.getColIndex(i)) +
        SudokuProblem.getCelIndex(i)
      box should be >= 243
      box should be < 324

      for(j <- 243 until 324) {
        array(i)(j) should be (j == box)
      }
    }
  }
}
