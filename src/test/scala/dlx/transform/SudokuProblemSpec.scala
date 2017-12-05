package dlx.transform

import dlx.SparseMatrix
import org.scalatest.{FlatSpec, Matchers}

/**
  * || _ 4 3 | 2 6 _ | _ _ _ ||     || 5 4 3 | 2 6 7 | 8 9 1 ||
  * || _ _ 6 | _ _ _ | _ _ 3 ||     || 2 1 6 | 8 4 9 | 7 5 3 ||
  * || _ 8 9 | 1 _ 5 | _ _ _ ||     || 7 8 9 | 1 3 5 | 4 6 2 ||
  * ||-------|-------|-------||     ||-------|-------|-------||
  * || _ 3 _ | 6 _ _ | _ _ 4 ||     || 9 3 2 | 6 7 8 | 5 1 4 ||
  * || 4 7 _ | _ 1 _ | _ 3 8 || <=> || 4 7 5 | 9 1 2 | 6 3 8 ||
  * || 8 _ _ | _ _ 4 | _ 7 _ ||     || 8 6 1 | 3 5 4 | 2 7 9 ||
  * ||-------|-------|-------||     ||-------|-------|-------||
  * || _ _ _ | 7 _ 6 | 1 8 _ ||     || 3 9 4 | 7 2 6 | 1 8 5 ||
  * || 1 _ _ | _ _ _ | 9 _ _ ||     || 1 2 7 | 5 8 3 | 9 4 6 ||
  * || _ _ _ | _ 9 1 | 3 2 _ ||     || 6 5 8 | 4 9 1 | 3 2 7 ||
  */
object SudokuExample1 {
  val grid = Array(
    Array[Byte](0, 4, 3, 2, 6, 0, 0, 0, 0),
    Array[Byte](0, 0, 6, 0, 0, 0, 0, 0, 3),
    Array[Byte](0, 8, 9, 1, 0, 5, 0, 0, 0),
    Array[Byte](0, 3, 0, 6, 0, 0, 0, 0, 4),
    Array[Byte](4, 7, 0, 0, 1, 0, 0, 3, 8),
    Array[Byte](8, 0, 0, 0, 0, 4, 0, 7, 0),
    Array[Byte](0, 0, 0, 7, 0, 6, 1, 8, 0),
    Array[Byte](1, 0, 0, 0, 0, 0, 9, 0, 0),
    Array[Byte](0, 0, 0, 0, 9, 1, 3, 2, 0)
  )

  val sol = Array(
    Array[Byte](5 ,4 ,3 ,2, 6, 7, 8, 9, 1),
    Array[Byte](2 ,1 ,6 ,8, 4, 9, 7, 5, 3),
    Array[Byte](7 ,8 ,9 ,1, 3, 5, 4, 6, 2),
    Array[Byte](9 ,3 ,2 ,6, 7, 8, 5, 1, 4),
    Array[Byte](4 ,7 ,5 ,9, 1, 2, 6, 3, 8),
    Array[Byte](8 ,6 ,1 ,3, 5, 4, 2, 7, 9),
    Array[Byte](3 ,9 ,4 ,7, 2, 6, 1, 8, 5),
    Array[Byte](1 ,2 ,7 ,5, 8, 3, 9, 4, 6),
    Array[Byte](6 ,5 ,8 ,4, 9, 1, 3, 2, 7)
  )
}

final class SudokuProblemSpec extends FlatSpec with Matchers {

  sealed trait SudokuSparseMatrix {
    val grid = new Array[Array[Byte]](9)
    for (i <- grid.indices) {
      grid(i) = Array.fill[Byte](9)(0)
    }

    val sparseMatrix: Array[Array[Boolean]] = SudokuProblem.convert(grid)
  }

  def getRowIndex(index: Int): Int = index / 81
  def getColIndex(index: Int): Int = (index % 81) / 9
  def getBoxColIndex(colIndex: Int): Int = colIndex / 3 * 9
  def getBoxRowIndex(rowIndex: Int): Int = rowIndex / 3 * 27
  def getBoxIndex(i: Int, j: Int): Int = getBoxRowIndex(i) + getBoxColIndex(j)
  def getCelIndex(index: Int): Int = index % 9

  /**
    * Helper Method
    *
    * @param ls
    * @param exp
    * @param f
    */
  def checkIndex(ls: List[Int], exp: Int)(f: Int => Int): Unit = {
    ls.foreach(x => f(x) should be(exp))
  }

  "celIndex" should "be contained" in {
    checkIndex(List(0, 8), 0)(SudokuProblem.celColumnIndexBy)
    SudokuProblem.celColumnIndexBy(9) should be(1)
    SudokuProblem.celColumnIndexBy(728) should be(80)
    intercept[IllegalArgumentException] {
      SudokuProblem.celColumnIndexBy(729)
    }
  }

  "rowIndex" should "be contained" in {
    SudokuProblem.rowColumnIndexBy(0) should be(81)
    SudokuProblem.rowColumnIndexBy(728) should be(161)
    intercept[IllegalArgumentException] {
      SudokuProblem.rowColumnIndexBy(729)
    }
  }

  "colIndex" should "be contained" in {
    checkIndex(List(0, 81), 162)(SudokuProblem.colColumnIndexBy)
    checkIndex(List(80, 728), 242)(SudokuProblem.colColumnIndexBy)
    intercept[IllegalArgumentException] {
      SudokuProblem.colColumnIndexBy(729)
    }
  }


  "boxIndex" should "be contained" in {
    checkIndex(List(0, 9, 18), 243)(SudokuProblem.boxColumnIndexBy)
    checkIndex(List(27, 36), 252)(SudokuProblem.boxColumnIndexBy)
    checkIndex(List(54, 63), 261)(SudokuProblem.boxColumnIndexBy)
    // Row 2
    SudokuProblem.boxColumnIndexBy(81) should be(243)
    // Row 3
    SudokuProblem.boxColumnIndexBy(162) should be(243)
    // Row 4
    checkIndex(List(243, 324), 270)(SudokuProblem.boxColumnIndexBy)

    SudokuProblem.boxColumnIndexBy(728) should be(323)
    intercept[IllegalArgumentException] {
      SudokuProblem.boxColumnIndexBy(729)
    }
  }

  "An empty Sudoku problem" should "be converted correctly" in new SudokuSparseMatrix {

    def checkOnes(i: Int, r: Range, v: Int): Unit = {
      for(j <- r) {
        sparseMatrix(i)(j) should be(j == v)
      }
    }

    sparseMatrix should have length 729
    for(i <- sparseMatrix.indices) {
      sparseMatrix(i) should have length 324
      //cells
      val cel = getRowIndex(i) * 9 + getColIndex(i)
      cel should (be >= 0 and be < 81)
      checkOnes(i, 0 until 81, cel)
      // rows
      val row = 81 + getRowIndex(i) * 9 + getCelIndex(i)
      row should (be >= 81 and be < 162)
      checkOnes(i, 81 until 162, row)
      // cols
      val col = 162 + getColIndex(i) * 9  + getCelIndex(i)
      col should (be >= 162 and be < 243)
      checkOnes(i, 162 until 243, col)
      // boxes
      val box = 243 + getBoxIndex(getRowIndex(i), getColIndex(i)) + getCelIndex(i)
      box should (be >= 243 and be < 324)
      checkOnes(i, 243 until 324, box)
    }
  }

  "An empty converted Sudoku problem" should "contains 4 ones in each row" in new SudokuSparseMatrix {
    for(i <- sparseMatrix.indices) {
      sparseMatrix(i).foldLeft(0)((acc, x) => if (x) acc + 1 else acc) should be(4)
    }
  }

  "An empty converted Sudoku problem" should "contains 1 ones in each column" in new SudokuSparseMatrix {
      for(j <- sparseMatrix(0).indices) {
        var acc = 0
        for(i <- sparseMatrix.indices) {
            if(sparseMatrix(i)(j)) acc += 1
        }
        acc should be(9)
    }
  }

  "row helper" should "be return valud values" in {
    SudokuProblem.row(0, 0, 1) should be(0)
    SudokuProblem.row(0, 0, 2) should be(1)
    SudokuProblem.row(0, 0, 9) should be(8)
    SudokuProblem.row(0, 1, 1) should be(9)
    SudokuProblem.row(0, 8, 1) should be(72)
    SudokuProblem.row(1, 0, 1) should be(81)
    SudokuProblem.row(8, 8, 1) should be(720)
    SudokuProblem.row(8, 8, 9) should be(728)
  }

  "colCel helper" should "be return valid values" in {
    SudokuProblem.colCel(0, 0) should be(0)
    SudokuProblem.colCel(0, 1) should be(1)
    SudokuProblem.colCel(0, 8) should be(8)
    SudokuProblem.colCel(1, 0) should be(9)
    SudokuProblem.colCel(8, 8) should be(80)
  }

  "colRow helper" should "be return valid values" in {
    SudokuProblem.colRow(0, 1) should be(81)
    SudokuProblem.colRow(0, 9) should be(89)
    SudokuProblem.colRow(1, 1) should be(91) // ?
    SudokuProblem.colRow(1, 9) should be(99) 
    SudokuProblem.colRow(8, 1) should be(153)
    SudokuProblem.colRow(8, 9) should be(161)
  }

  "colCol helper" should "be return valid values" in {
    SudokuProblem.colCol(0, 1) should be(162)
    SudokuProblem.colCol(0, 9) should be(170)
    SudokuProblem.colCol(1, 1) should be(171)
    SudokuProblem.colCol(8, 1) should be(234)
    SudokuProblem.colCol(8, 9) should be(242)
  }

  "colBox helper" should "be return valud values" in {
    SudokuProblem.colBox(0, 0, 1) should be(243)
    // TODO induction tests incomplete
  }

  "Sudoku example 1 solution" should "be a valid solution" in {
    SudokuProblem.solutionGridCheck(SudokuExample1.sol) should be (true)
  }

  "Sudoku example 1 solution" should "be completely transformed correctly" in {
    val sm = SudokuProblem.convert(SudokuExample1.sol)

    SudokuProblem.unconvert(sm) should be (SudokuExample1.sol)
  }

  "26 pieces in a Sudoku game example 1" should "be converted correctly" in {
    val sparseMatrix = SudokuProblem.convert(SudokuExample1.grid)

    SudokuProblem.unconvert(sparseMatrix) should be (SudokuExample1.grid)
  }
}
