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
    * @param O
    * @param sol
    */
  def search(O: ListBuffer[Data], sol: ListBuffer[Data]): Unit = {
    if (h.r == h) {
      // solution found
      O.copyToBuffer(sol)
      return
    }

    val c = chooseColumn()

    coverColumn(c)
    var r = c.d
    while (r != c) {
      O += r
      var j = r.r
      while(j != r) {
        coverColumn(j.c)
        j = j.r
      }

      search(O, sol)
      j = r.l
      while (j != r) {
        uncoverColumn(j.c)
        j = j.l
      }

      O -= r
      r = r.d
    }

    uncoverColumn(c)
  }

  /**
    *
    * @param O
    * @return
    */
  def convertSolutionToIndexList(sol: ListBuffer[Data]):Array[Array[Int]] = {

    var solutionAsIndexLists = Array[Array[Int]]()

    sol.foreach(O => {
      var r = O
      var indexRow = Array[Int]()
      do {
        indexRow :+= r.c.n
        r = r.r
      } while (r != O)

      solutionAsIndexLists :+= indexRow
    })

    solutionAsIndexLists
  }

  def solve(): Array[Array[Int]] = {
    val O: ListBuffer[Data] = new ListBuffer[Data]()
    val sol: ListBuffer[Data] = new ListBuffer[Data]()

    search(O, sol)

    convertSolutionToIndexList(sol)
  }
}
