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
  * @todo refactor the class with less mutation var and use a companion object
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

    Data.fold(0, c, c.d)((acc, i) => {
      Data.fold(0, i, i.r)((acc, j) => {
        j.d.u = j.u
        j.u.d = j.d
        j.c.s -= 1
        assert(j.c.s >= 0)
        (acc, j.r)
      })

      (acc, i.d)
    })
  }

  /**
    *
    * @param c
    */
  def uncoverColumn(c: Column): Unit = {
    Data.fold(0, c, c.u)((acc, i) => {
      Data.fold(0, i, i.l)((acc, j) => {
        j.c.s += 1
        j.d.u = j
        j.u.d = j

        (acc, j.l)
      })

      (acc, i.u)
    })

    c.r.l = c
    c.l.r = c
  }

  /**
    * Choose column with the lowest number of 1s
    *
    * @return
    */
  private def chooseColumn(): Column = {
    val cur = h.r.asInstanceOf[Column]
    Data.fold(cur, h, cur)((cur, j) => {
      val c = j.asInstanceOf[Column]
      if(c.s < cur.s) {
        (c, j.r)
      } else {
        (cur, j.r)
      }
    })
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
      Data.fold(0, c, c.d)((acc, r) => {
        curSol += r
        Data.fold(0, r, r.r)((acc, j) => {
          coverColumn(j.c)
          (acc, j.r)
        })

        search(curSol, sol)
        Data.fold(0, r, r.l)((acc, j) => {
          uncoverColumn(j.c)
          (acc, j.l)
        })

        curSol -= r
        (acc, r.d)
      })

      uncoverColumn(c)
    }
  }

  /**
    * Solution found it would be converted into column indexes representing the 1s in the row
    *
    * @param sol
    * @return
    */
  private def convertSolutionToIndexList(sol: ListBuffer[Data]): Array[Array[Int]] = sol.map(curSol =>
    Data.fold(Array[Int](curSol.c.n),curSol, curSol.r)((arr, r) =>
      (arr :+ r.c.n, r.r))
  ).toArray

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
          val res = row.forall(j => matrix(i)(j))
          val rowSize = matrix(i).foldLeft[Int](0) { (a, x) => if(x) a + 1 else a }
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
