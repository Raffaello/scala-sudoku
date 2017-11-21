package dlx

/**
  * Sparse Matrix A, DLX data structure for exact cover problem
  *
  * @param matrix A m,n size of zeros and ones (false, true)
  */
final class SparseMatrix(val matrix: Array[Array[Boolean]]) {

  private val m: Int = matrix.length
  if (m == 0) throw new IllegalArgumentException("matrix has zero rows")
  private val n: Int = matrix(0).length
  if (n == 0) throw new IllegalArgumentException("matrix has zero columns")
  for (i <- matrix.indices) {
    if (matrix(i).length != n) throw new IllegalArgumentException(s"column $i has a different size of $n")
  }

  /**
    * Build the Sparse Matrix and return the root column header
    *
    * @return
    */
  private def build(): Column = {
    val root: Column = new Column(-1, -1, null, null, null, null, null)
    var cur: Column = root
    var curUp: Data = null
    var d: Data = null
    var dl: Data = null
    var d0: Data = null

    /**
      * 1st data in this column
      *
      * @param c column header
      * @return
      */
    def firstDataCell(c: Column): Data = {
      val d = new Data(null, null, c, c, c)
      c.d = d
      c.u = d
      d.l = d
      d.r = d

      d
    }

    for {
      j <- 0 until n
      i <- 0 until m
      if matrix(i)(j)
    } {
      if (cur.n != j) {
        // 1st 1 in this column, create column cell
        val c = new Column(0, j, cur, root, null, null)
        c.c = c
        cur.r = c
        root.l = c

        if (null == root.r) {
          // the 1st column header
          root.r = c
          c.l = root
        }

        d = firstDataCell(c)
        d0 = d // this is wrong it will be change for each new column! (need 2 for loops)
        cur = c

      } else {

        d = new Data(dl, d0, curUp, cur, cur)
        curUp.d = d
      }

      curUp = d // only when i++
      dl = d // this is wrong too only when j++ should be done
    }

    // by definition of the matrix these should be never null
    assert(null != cur)
    assert(null != curUp)
    assert(null == root.d)
    assert(null == root.u)

    root
  }

  val root: Column = build()

  /**
    *
    * @param j Column Index
    * @return
    */
  private def buildSparseColumn(j: Int, colHeader: Column): Column = {

    var leftData: Data = null
    var d0: Data = null

    for (i <- 0 until m) {
      if (matrix(i)(j)) {
        colHeader.s += 1
        val d = new Data()
        d.c = colHeader

        d.u = d
        d.d = d

        if (d0 == null) {
          d0 = d
        }

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

//      colHeader = colHeader.r
    }

    //      if (d0 != null) {
    //        assert(d0 == leftData.r)
    //        upData = d0 //back to the first
    //        if (d00 == null) {
    //          d00 = upData
    //          d00.d = upData
    //        }
    //      }

    root
  }

  //  val header: Column = init(initColumnHeaders(initHeader))
}
