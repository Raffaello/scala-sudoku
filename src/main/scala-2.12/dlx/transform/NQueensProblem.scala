package dlx.transform

/**
  * N=8 generalizing later
  * CheckBoard representation matrix 8*8 of booleans true=queen is present, false otherwise
  * SparseMatrix is 64*42
  *
  * constraints: N Ranks, N Files, 2N-2 Diagonal, 2N-2 RDiagonal
  *
  * @todo generalize N
  */
object NQueensProblem {
  private[this] val N = 8
  private[this] val m = N * N
  private[this] val n = 6 * (N - 1)

  def rankColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    rowIndex / N
  }

  def fileColumnIndexBy(rowIndex: Int): Int = {
    require(rowIndex < m && rowIndex >= 0)
    N + rowIndex % N
  }

  def buildDiagABy(array: Array[Array[Boolean]], diagIndex: Int): Unit = {
    require(diagIndex >= 0 && diagIndex < 2*N-3)

    if (diagIndex < N - 1) {
      // 0..6 (A1..A7)
      val offset = N*2
      for(i <- diagIndex until array.length by N + 1) {
        array(i)(offset + diagIndex) = true
      }
    } else {
      // 7..12 (A9..A14) offset = N*2 + N-1
      val offset = N*2
      val ioffset = diagIndex - N + 1
      for(i <- (array.length-1) to N by -(N + 1)) {
        array(i - ioffset)(offset + diagIndex) = true
      }

      val a=0
    }
  }

  def buildDiagBBy(array: Array[Array[Boolean]], diagIndex: Int): Unit = {
    require(diagIndex >= 0 && diagIndex < 2*N-3)
    val offset = (N * 4) - 3

    if(diagIndex < N - 1) {
      // 0..6 (B1..B7)
      for(i <- N-1+diagIndex until array.length by (N - 1)) {
        array(i)(offset + diagIndex) = true
      }
    } else {
      //7..12 (B9..B14)
      val ioffset = diagIndex - N + 1
      for (i <- (array.length-N) until 0 by -(N-1)) {
        array(i - ioffset)(offset + diagIndex) = true
      }
    }
  }

  def buildArray(): Array[Array[Boolean]] = {
    val array = new Array[Array[Boolean]](m)

    for (i <- array.indices) {
      val a = Array.fill[Boolean](n)(false)
      a(rankColumnIndexBy(i)) = true
      a(fileColumnIndexBy(i)) = true
      array(i) = a
    }

    // build Diagonals A, B
    for(i <- 0 until 2 * N - 3) {
      buildDiagABy(array, i)
      buildDiagBBy(array, i)
    }

    array
  }

  def convert(checkBoard: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    buildArray()
    // todo: filling the checkboard

  }

  def unconvert(sparseMatrix: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val checkBoard = Array.ofDim[Boolean](N, N)

    for {
      i <- checkBoard.indices
      j <- checkBoard(i).indices
    } {
      val rowValue  = i*N + j
      val rankValue = i % N
      val fileValue = N + j
      //val diagAValue = 0
      //val diagBValue = 0
      if (sparseMatrix(rowValue)(rankValue)
        && sparseMatrix(rowValue)(fileValue)
        //&& sparseMatrix(rowValue)(diagAValue)
        //&& sparseMatrix(rowValue)(diagBValue)
      ) {
        checkBoard(i)(j) = true
      } else {
        checkBoard(i)(j) = false
      }
    }

    checkBoard
  }

  def unconvert(solution: Array[Array[Int]]): Array[Array[Boolean]] = {
    val checkBoard = Array.ofDim[Boolean](N, N)
    //todo
    checkBoard
  }
}
