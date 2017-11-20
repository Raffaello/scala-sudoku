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
          data.c = curCol
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
  val h:Column = buildHeader()
  val mat = buildMatrix()

  private def buildMatrix() = {
    var leftData:Data = null

    for(i <- 0 until n) {
      var d0:Data = null
      var colHeader = root
      for(j <- 0 until m) {
        if(0 != matrix(i)(j)) {
          colHeader.s += 1
          val d = new Data()
          d.c = colHeader

          if (null == colHeader.u) {
            colHeader.u = d
            d.u = d
          } else {
            d.u = colHeader.u
          }

          colHeader.u = d
          if(null == colHeader.d) {
            colHeader.d = d
            d.d = d
          } else {
            d.d = colHeader.d
          }

          if (d0 == null) {
            d0 = d
          }

          d.r = d0
          d0.l = d

          if (leftData != null) {
            leftData.r = d
            d.l = leftData
          } else {
            d.l = d
          }

//          if (upData != null) {
//            upData.d = d
//            d.u = upData
//            upData = upData.r
//            if(d00 != null) {
//              d00.u = d
//              d.d = d00
//            }
//          } else {
//            d.u = d
//            d.d = d
//          }

          leftData = d
        }

        colHeader = colHeader.r.asInstanceOf[Column]
      }

//      if (d0 != null) {
//        assert(d0 == leftData.r)
//        upData = d0 //back to the first
//        if (d00 == null) {
//          d00 = upData
//          d00.d = upData
//        }
//      }
    }

    root
  }

  val header: Column = init(initColumnHeaders(initHeader))
}
