package dlx.transform

import scala.annotation.tailrec
import cats.implicits._

/**
  * Universe 0..9 (0 means empty cell, no value)
  * Grid 9x9 of 9 values => 9x9x9 = 729 rows
  * Constraints (columns):
  *  - Row-Column (cell) => contain 1 number 81 cells
  *  - Row-Number (row)  => 81
  *  - col-Number (col)  => 81
  *  - box-number (box)  => 81
  *
  *  729 * 324 full problem with no clues
  *
  * Adding a clue (number in a cell)
  * remove constrain rows to enforce the clue to be in the solution eg:
  * R1C1#1 => no need of R1C1#2..9, R2..9C1#1, R1,C2..9#1, R1..3C1..3#1-R1C1(box)
  *           8 + 8 + 8 + 8 = 32 less (or set to zero)
  * ===> for each clue adding only 4 constraints
  *
  * Exact Cover Matrix [[http://www.stolaf.edu/people/hansonr/sudoku/exactcovermatrix.htm]]
  *
  * @todo generalizing, now is n*n = 9
  */
object SudokuProblem {

  class IllegalSolution(
    private val message: String = "",
    private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  /** n*n * n*n n*n */
  private val m = 729
  /** 4 * n*n */
  private val n = 324

  /**
    * increase by 1 each 9 rows
    * [0..80]
    * @param rowIndex
    * @return
    */
  def celColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    rowIndex / 9
  }

  /**
    * increase by 1 each 1 rows, repeat each 9 rows and then increase of 9
    * [81..161]
    * @param rowIndex
    * @return
    */
  def rowColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    81 + rowIndex % 9 + (rowIndex / 81) * 9
  }

  /**
    * increase by 1 each rows, repeat 81 rows and then restart
    * [162..242]
    * @param rowIndex
    * @return
    */
  def colColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    162 + (rowIndex % 81)
  }

  /**
    * increase by 1 each rows, repeat and after 3 times increase 9, after 3 times(9 total) restart
    * increase by 1 each rows,
    * increase by 9 every 3 columns 3 times and repeat
    * increase by 9 every 3 rows 3 times and repeat
    * [243..341]
    * @param rowIndex
    * @return
    */
  def boxColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    243 + rowIndex % 9 + (rowIndex / 27) % 3 * 9 + (rowIndex / 243) * 27
  }

  private def buildArray(): Array[Array[Boolean]] = {
    val array = new Array[Array[Boolean]](m)

    for(i <- array.indices) {
      val a = Array.fill[Boolean](n)(false)
      a(celColumnIndexBy(i)) = true
      a(rowColumnIndexBy(i)) = true
      a(colColumnIndexBy(i)) = true
      a(boxColumnIndexBy(i)) = true
      array(i) = a
    }

    array
  }

  /**
    * @param i row
    * @param j column
    * @param v value [1,9]
    * @return
    */
  def row(i: Int, j: Int, v: Byte): Int = {
    require(v >= 1 && v <= 9 && i >= 0 && i < 9 && j >= 0 && j < 9)
    //    (v-1) + 9 * (i*9 + j)
    i * 81 + j * 9 + (v-1)
  }

  def colCel(i: Int, j: Int): Int = {
    require(i >= 0 && i < 9 && j >= 0 && j < 9)
    i * 9 + j
  }

  def colRow(i: Int, v: Byte): Int = {
    require(v >= 1 && v <= 9 && i >= 0 && i < 9)
    //81 + (v-1) = 80+v
    80 + v + (i*9)
  }

  def colCol(j: Int, v: Byte): Int = {
    require(v >= 1 && v <= 9 && j >= 0 && j < 9)
    //162 +v-1
    161 + v +(j*9)
  }

  def colBox(i: Int, j: Int, v: Byte): Int = {
    require(v >= 1 && v <= 9 && i >= 0 && i < 9 && j >= 0 && j < 9)
    //243 + v-1
    242 + v + (j/3*9) + (i/3*27)
  }

  /**
    * insert the value from the grid coordinate position into the sparseMatrix
    *
    * @param i grid row
    * @param j grid column
    * @param value number [1,9p
    */
  def insertValuefromGrid(sparseMatrix: Array[Array[Boolean]], i: Int, j: Int, value: Byte): Unit = {
    require(i >= 0 && j >= 0 && i < m && j < n && value <= 9 && value >= 1)

    val r = row(i, j, value)
    val cCel = colCel(i, j)
    // Actually this 4 values should be true and the other relative 8 to be false
    assert(sparseMatrix(r)(cCel))
    assert(sparseMatrix(r)(colRow(i, value)))
    assert(sparseMatrix(r)(colCol(j, value)))
    assert(sparseMatrix(r)(colBox(i, j, value)))

    (1 to 9).filter(_ =!= value).foreach{vv =>
      val v = vv.toByte
      val r = row(i, j, v)
      sparseMatrix(r)(cCel) = false
      sparseMatrix(r)(colRow(i, v)) = false
      sparseMatrix(r)(colCol(j, v)) = false
      sparseMatrix(r)(colBox(i, j, v)) = false
    }
  }

  /**
    * Convert from a normal Sudoku grid representation
    * to the Sparse Matrix format
    * First build the empty SparseMatrix equivalent to an empty Sudoku Grid
    * Then modify it accordingly to the numbers present in the grid
    * eg 1 in the top leftmost cell (r1 c1 v1)
    * will translate The Sparse Matrix with 0s values in:
    * (0,0),   cell
    * (0,81),  row
    * (0,162), col
    * (0,243), box TL
    *
    * eg 5 in r1,c1,v1
    * (4,0),    cell
    * (4,85),   row
    * (4,166),  col
    * (4,247), box TL
    *
    * general v in r1 c1 v is v-1 zero based
    * (v, 0),     cell
    * (v, 81+v),  row
    * (v, 162+v), col
    * (v, 243+v), box TL
    *
    * general v in r c r,c,v are -1 (zero based)
    * (v+c*9+r*81, c+r*9),    cell
    * (v+c*9+r*81, 81+v*r*9), row
    * (v+c*9+r*81, 162+v+c*9) col
    * (v+c*9+r*81, v+c/3*9 + r/3*27)
    *
    * @param grid
    * @return
    */
  def convert(grid: Array[Array[Byte]]): Array[Array[Boolean]] = {
    val sparseMatrix = buildArray()
    for {
      i <- grid.indices
      j <- grid(i).indices
      if grid(i)(j) > 0
    } {
        insertValuefromGrid(sparseMatrix, i, j, grid(i)(j))
      }

    sparseMatrix
  }

  /**
    * Exact Cover Problem to Sudoku Grid
    *
    * @param sparseMatrix
    * @return
    */
  def unconvert(sparseMatrix: Array[Array[Boolean]]): Array[Array[Byte]] = {
    val grid = Array.ofDim[Byte](9,9)
    for { i <- grid.indices
          j <- grid(i).indices
    } { for (value <- 1 to 9) {
        val v = value.toByte
        val rowValues = row(i, j, v)
        if (sparseMatrix(rowValues)(colCel(i, j)) &&
          sparseMatrix(rowValues)(colRow(i, v)) &&
          sparseMatrix(rowValues)(colCol(j, v)) &&
          sparseMatrix(rowValues)(colBox(i, j, v))
        ) {
          grid(i)(j) = (v + grid(i)(j)).toByte
        }
      }

      if (grid(i)(j) === 45) grid(i)(j) = 0
    }

    grid
  }

  def unconvert(solution: Array[Array[Int]]): Array[Array[Byte]] = {
    val grid = Array.ofDim[Byte](9, 9)

    for (solRow <- solution) {
      val ss = solRow.sorted

      //get grid row and column
      // invColCel
      val i =  ss(0) / 9
      val j =  ss(0) % 9
      // invValue from row
      val v = (ss(1) - 80 - i * 9).toByte

      if(colCel(i, j) =!= ss(0) ||
      colRow(i, v) =!= ss(1) ||
      colCol(j, v) =!= ss(2) ||
      colBox(i, j, v) =!= ss(3)) {
        throw new IllegalSolution(s"doesn't match row=$i -- col=$j -- value=$v in (${ss.toList.toString})")
      }

      grid(i)(j) = v
    }

    grid
  }

  def solutionGridCheck(grid: Array[Array[Byte]]): Boolean = {
    @tailrec def rowCheck(start: Byte, end: Byte, res: Boolean): Boolean = {
      val ret = res && grid(start).sum == 45
      if (!ret || start == end) ret
      else rowCheck((start + 1).toByte, end, ret)
    }

    @tailrec def colCheck(start: Byte, end: Byte, res: Boolean): Boolean = {
      val ret = grid.foldLeft(0)((acc, arr) => arr(start) + acc) == 45 && res
      if (!ret || start == end) ret
      else colCheck((start + 1).toByte, end, ret)
    }

    @tailrec def boxesCheck(start: Byte, end: Byte, res: Boolean): Boolean = {
      def subgrids(i: Int): Boolean = {
        val i3 = i * 3
        val subgrid = grid.slice(i3, i3 + 3)
        @tailrec def subgridsSumCheck(res: Boolean, j: Int): Boolean = {
          val j3 = j * 3
          val ret = res && subgrid.map(x => x.slice(j3, j3 + 3))
            .foldLeft(0)((acc, arr) => arr.sum + acc) == 45
          if (!ret || j == 2) ret
          else subgridsSumCheck(ret, j + 1)
        }

        subgridsSumCheck(true, 0)
      }
      val ret = res && subgrids(start)
      if (!ret || start == end) ret
      else boxesCheck((start + 1).toByte, end, ret)
    }

    @tailrec def unitGridCheck(start: Byte, end: Byte, res: Boolean): Boolean = {
      val ret = res && grid.foldLeft[Int](0)((acc, arr) => acc + arr.count(x => x == start)) == 9
      if (start == end || !ret) ret
      else unitGridCheck((start + 1).toByte, end, ret)
    }

    unitGridCheck(1, 9, true) &&  rowCheck(0, 8, true) &&
      colCheck(0, 8, true) && boxesCheck(0, 2, true)
  }
}
