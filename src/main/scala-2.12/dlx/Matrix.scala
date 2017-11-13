package dlx

/**
  * Matrix A, DLX data structure
  *
  * @todo redo it i a better way the c field it is not initialized properly
  *       also use composition over inheritance for the data and column objects.
  *
  * @param matrix A m,n size
  */
final class Matrix(val matrix: Array[Array[Int]]) {

  private val m: Int = matrix.length
  if (m == 0) throw new IllegalArgumentException("matrix has zero rows")
  private val n: Int = matrix(0).length
  if (n == 0) throw new IllegalArgumentException("matrix has zero columns")
  for (i <- matrix.indices) {
    if (matrix(i).length != n) throw new IllegalArgumentException(s"column $i has a different size of $n")
  }

  private def initRoot: Column = {
    val root: Column = new Column(0, -1)
    root.l = root
    root.r = root
    root.u = root
    root.d = root

    root
  }

  private def initColumnHeaders(header: Column): Column = {
    for (j <- 0 until n) {
      val col: Column = new Column(0, j)
      col.l = header.l
      col.r = header
      col.u = col
      col.d = col
      header.l.r = col
      header.l = col
    }

    header
  }

  /**
    * Init the matrix double ways linked (vertically and horizontally)
    *
    * @param h root header
    * @return
    */
  private def init(h: Column): Column = {
    for (i <- 0 until m) {
      var curCol = h.r
      var dataRow = Array[Data]()

      // data row and vertical linking (up, down)
      for(j <- 0 until n) {
        if (matrix(i)(j) != 0) {
          val data = new Data()
          data.l = data
          data.r = data
          data.u = curCol.u
          data.d = curCol
          curCol.u.d = data
          curCol.u = data
          curCol.asInstanceOf[Column].s += 1
          dataRow :+= data
        }

        curCol = curCol.r
      }

      // horizontal linking (left, right)
      val rowHead = dataRow(0)
      for (j <- 1 until dataRow.length) {
        dataRow(j).l = rowHead.l
        dataRow(j).r = rowHead
        rowHead.l.r = dataRow(j)
        rowHead.l = dataRow(j)
      }
    }

    h
  }

  val header: Column = init(initColumnHeaders(initRoot))
}
