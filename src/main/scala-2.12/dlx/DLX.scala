package dlx

import scala.collection.mutable.ListBuffer

/**
  * Algorithm X [[https://arxiv.org/abs/cs/0011047]]
  *
  * @note The listHeader field of each object points
  *       to the column object at the head of the relevant column
  *       The A matrix need to represent the Exact Cover Problem,
  *       You can use some transformation to represent into 0s and 1s if required.
  *
  * @param matrix Exact Cover Matrix A made of 0s and 1s
  */
class DLX(var matrix: Array[Array[Boolean]]) {

  val sparseMatrix = new SparseMatrix(matrix)
  val h: Column = sparseMatrix.root

  /**
    *
    * @param c header column
    */
  def coverColumn(c: Column): Unit = {
    c.r.l = c.l
    c.l.r = c.r

    var i = c.d
    while(i != c) {
      var j = i.r
      while (j != i) {
        j.d.u = j.u
        j.u.d = j.d
        j.c.s -= 1
        assert(j.c.s >= 0)

        j = j.r
      }

      i = i.d
    }
  }

  /**
    *
    * @param c
    */
  def uncoverColumn(c: Column): Unit = {
    var i = c.u
    while(i != c) {
      var j = i.l
      while(j != i) {
        j.c.s = j.c.s + 1
        j.d.u = j
        j.u.d = j

        j = j.l
      }

      i = i.u
    }

    c.r.l = c
    c.l.r = c
  }

  /**
    * Choose column with the lowest number of 1s
    *
    * @return
    */
  private def chooseColumn(): Column = {
    var j = h.r.asInstanceOf[Column]
    var col = j
    var s = j.s

    while (j != h) {
      if (j.s < s) {
        col = j
        s = j.s
      }

      j = j.r.asInstanceOf[Column]
    }

    col
  }

  /**
    * @param curSol
    * @param sol
    */
  private def search(curSol: ListBuffer[Data], sol: ListBuffer[Data]): Unit = {
    if (h.r == h) {
      // solution found
      curSol.copyToBuffer(sol)
    } else {

      val c = chooseColumn()

      coverColumn(c)
      var r = c.d
      while (r != c) {
        curSol += r
        var j = r.r
        while (j != r) {
          coverColumn(j.c)
          j = j.r
        }

        search(curSol, sol)
        j = r.l
        while (j != r) {
          uncoverColumn(j.c)
          j = j.l
        }

        curSol -= r
        r = r.d
      }

      uncoverColumn(c)
    }
  }

  /**
    * Solution found it would be converted into column indexes representing the 1s in the row
    *
    * @param sol
    * @return
    */
  private def convertSolutionToIndexList(sol: ListBuffer[Data]): Array[Array[Int]] = {

    var solutionAsIndexLists = Array[Array[Int]]()

    sol.foreach(curSol => {
      var r = curSol
      var indexRow = Array[Int]()
      do {
        indexRow :+= r.c.n
        r = r.r
      } while (r != curSol)

      solutionAsIndexLists :+= indexRow
    })

    solutionAsIndexLists
  }

  /**
    * it convert the indexList into the respective row index
    *
    * @param indexList indexList solution column based of 1s, organized in rows
    * @return
    */
  def convertIndexListToRows(indexList : Array[Array[Int]]): Set[Int] = {
    val sol : ListBuffer[Int] = ListBuffer[Int]()

    indexList.foreach(row => {
        for(i <- matrix.indices) {
          var res = true
          row.foreach(j => {
            res &&= matrix(i)(j)
          })

          val rowSize = matrix(i).foldLeft[Int](0) { (a, x) => if(x) a+1 else a }
          if (res && rowSize == row.length) {
            sol += i
          }
      }
    })

      sol.toSet
  }

  /**
    * solve th Exact Cover Problem
    * @return
    */
  def solve(): Array[Array[Int]] = {
    val curSol: ListBuffer[Data] = new ListBuffer[Data]()
    val sol: ListBuffer[Data] = new ListBuffer[Data]()

    search(curSol, sol)

    convertSolutionToIndexList(sol)
  }
}
