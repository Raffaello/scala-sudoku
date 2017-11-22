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
    * @param col header column
    */
  def coverColumn(col: Column): Unit = {
    col.r.l = col.l
    col.l.r = col.r

    var i = col.d
    while(i != col) {
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
    * @param col
    */
  def uncoverColumn(col: Column): Unit = {
    var i = col.u
    while(i != col) {
      var j = i.l
      while(j != i) {
        j.c.s = j.c.s + 1
        j.d.u = j
        j.u.d = j
        j = j.l
      }

      i = i.u
    }

    col.r.l = col
    col.l.r = col
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
    * @todo redo with k parameter and in a tail recursive way
    *
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

  def solve(): Array[Array[Int]] = {
    val O: ListBuffer[Data] = new ListBuffer[Data]()
    val sol: ListBuffer[Data] = new ListBuffer[Data]()

    search(O, sol)

    var solutionAsIndexLists = Array[Array[Int]]()
    sol.foreach(row => {
      var myCol = row
      var flag = true
      var indexRow = Array[Int]()
      while (flag) {
        indexRow :+= myCol.c.n
        if (myCol.r == row)
          flag = false
        else
          myCol = myCol.r
      }
      solutionAsIndexLists :+= indexRow
    })

    solutionAsIndexLists
  }
}
