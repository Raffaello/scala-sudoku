package dlx.transform

import scala.annotation.tailrec

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
      val cel = celColumnIndexBy(i)
      val row = rowColumnIndexBy(i)
      val col = colColumnIndexBy(i)
      val box = boxColumnIndexBy(i)
      // cell
      a(cel) = true
      //row
      a(row) = true
      //col
      a(col) = true
      //box
      a(box) = true

      array(i) = a
    }

    array
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
    val row = i * 81 + j * 9 + value
    val cCel = i * 9 + (j + 1)
    val cRow = 81 + value + (i * 9)
    val cCol = 162 + value + (j * 9)
    val cBox = 243 + value + (j/3*9) + (i/3*9)

    sparseMatrix(row)(cCel) = false
    sparseMatrix(row)(cRow) = false
    sparseMatrix(row)(cCol) = false
    sparseMatrix(row)(cBox) = false
  }

  def convert(grid: Array[Array[Byte]]): Array[Array[Boolean]] = {
    val sparseMatrix = buildArray()
    // build the grid
    for {
      i <- grid.indices
      j <- grid(i).indices
      if grid(i)(j) > 0
    } {
        insertValuefromGrid(sparseMatrix, i, j, grid(i)(j))
      }

    sparseMatrix
  }

  def unconvert(sparseMatrix: Array[Array[Boolean]]): Array[Array[Byte]] = {
    val grid = Array.ofDim[Byte](9,9)
    for {
      i <- grid.indices
      j <- grid(i).indices
    } {
        // TODO
    }

    grid
  }

  def unconvert(solution: Array[Array[Int]]): Array[Array[Byte]] = ???

  def gridCheck(grid: Array[Array[Byte]]): Boolean = {
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
