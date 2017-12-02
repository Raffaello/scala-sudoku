package dlx.transform

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
    require(rowIndex < m)
    rowIndex / 9
  }

  /**
    * increase by 1 each 1 rows, repeat each 9 rows and then increase of 9
    * [81..161]
    * @param rowIndex
    * @return
    */
  def rowColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m)
    81 + rowIndex % 9 + (rowIndex / 81) * 9
  }

  /**
    * increase by 1 each rows, repeat 81 rows and then restart
    * [162..242]
    * @param rowIndex
    * @return
    */
  def colColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m)
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
    require(rowIndex < m)
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

  def convert(grid: Array[Array[Byte]]): Array[Array[Boolean]] = {
    val sparseMatrix = buildArray()
    // build the grid

    sparseMatrix
  }

  def uncovert(): Array[Array[Byte]] = ???
}
