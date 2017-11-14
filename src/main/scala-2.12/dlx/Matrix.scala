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


  /**
    * @deprecated
    *
    * @return
    */
  private def initHeader: Column = {
    val header: Column = new Column(0, -1)
    header.l = header
    header.r = header
    header.u = header
    header.d = header

    header
  }

  /**
    * @deprecated
    *
    * @param header
    * @return
    */
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
    * @deprecated
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

  /**
    * build the column header
    */
  private def buildHeader():Column = {
    var lh: Column = new Column(0,0)
    val h0: Column = lh

    for(j <- 1 until m) {
      val rh: Column = new Column(0, j)
      rh.l = lh
      lh.r = rh
      lh = rh
    }

    lh.r = h0
    h0.l = lh

    h0

  }

  val root:Column = buildHeader()

  private def build() = {
    for(i <- 0 until n) {
      for(j <- 0 until m) {
        if(0 != matrix(i)(j)) {
          // todo
        }
      }
    }
  }

  val header: Column = init(initColumnHeaders(initHeader))
}
