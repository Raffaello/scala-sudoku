package dlx

import scala.collection.mutable.ListBuffer

/**
  * @note The listHeader field of each object points
  *       to the column object at the head of the relevant column
  */

/**
  * Algorithm X [[https://arxiv.org/abs/cs/0011047]]
  *
  * @param root
  */
class DLX(var matrix: Array[Array[Int]]) {

  val h: Column = new Matrix(matrix).header

  def coverColumn(col: Column): Unit = {
    col.l.r = col.l
    col.r.l = col.l

    var myRow = col.d
    while(myRow != col) {
      var myCol = myRow.r
      while (myCol != myRow) {
        myCol.u.d = myCol.d
        myCol.d.u = myCol.u
        myCol.asInstanceOf[Column].s -= 1
        assert(myCol.asInstanceOf[Column].s >= 0)

        myCol = myCol.r
      }

      myRow = myRow.d
    }
  }

  def unconverColumn(col: Column): Unit = {
    var myRow = col.u
    while(myRow != col) {
      var myCol = myRow.l
      while(myCol != myRow) {
        myCol.u.d = myCol
        myCol.d.u = myCol
        myCol.asInstanceOf[Column].s += 1
        myCol = myCol.l
      }

      myRow = myRow.u
    }

    col.l.r = col
    col.r.l = col
  }

  /**
    * Choose column with the lowest number of 1s
    *
    * @return
    */
  private def chooseColumn(): Column = {
    var choose = h.r
    var col = h.r

    while (col != h) {
      choose = if (col.asInstanceOf[Column].s < choose.asInstanceOf[Column].s) col else choose
      col = col.r
    }

    choose.asInstanceOf[Column]
  }

  def search(O: ListBuffer[Data], sol: ListBuffer[Data]): Unit = {
    if (h.r == h) {
      // solution found
      O.copyToBuffer(sol)
    }

    val c = chooseColumn()
    if (c.s == 0) {
      return
    }

    coverColumn(c)
    var myRow = c.d
    while (myRow != c) {
      O += myRow
      var myCol = myRow.r
      while(myCol != myRow) {
        coverColumn(myCol.c.asInstanceOf[Column])
        myCol = myCol.r
      }

      search(O, sol)
      myCol = myRow.l
      while (myCol != myRow) {
        unconverColumn(myCol.c.asInstanceOf[Column])
        myCol = myCol.l
      }

      O -= myRow
      myRow = myRow.d
    }

    unconverColumn(c)
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
        indexRow :+= myCol.c.asInstanceOf[Column].n
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
