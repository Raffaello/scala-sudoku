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
  * Columns
  */
object SudokuProblem {
  private val m = 729
  private val n = 324

  def getRowIndex(index: Int): Int = index / 81
  def getColIndex(index: Int): Int = (index % 81) / 9
  def getBoxColIndex(colIndex: Int): Int = colIndex / 3 * 9
  def getBoxRowIndex(rowIndex: Int): Int = rowIndex / 3 * 27
  def getBoxIndex(i: Int, j: Int): Int = getBoxRowIndex(i) + getBoxColIndex(j)
  def getCelIndex(index: Int): Int = index % 9

  private def buildArray(): Array[Array[Boolean]] = {
    val array = new Array[Array[Boolean]](m)

    for(i <- array.indices) {
      val row = getRowIndex(i)//i/81
      val col = getColIndex(i)//(i%81)/9
      val box = getBoxIndex(row, col)//(col/3)*9
      val cel = getCelIndex(i)//i%9
      val a = Array.fill[Boolean](n)(false)

      // cell
      a(row * 9 + col) = true
      //row
      a(81 + row + cel) = true
      //col
      a(162 + col + cel) = true
      //box
      a(243 + box + cel) = true

      array(i) = a
    }

    array
  }

  def convert(grid: Array[Array[Byte]]): Array[Array[Boolean]] = {
    buildArray()
  }
}
