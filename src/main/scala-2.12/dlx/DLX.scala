package dlx

import scala.collection.mutable.ListBuffer

/**
  * Algorithm X [[https://arxiv.org/abs/cs/0011047]]
  *
  * @note The listHeader field of each object points
  *       to the column object at the head of the relevant column
  *
  * @param matrix
  */
class DLX(var matrix: Array[Array[Boolean]]) {

  val h: Column = new SparseMatrix(matrix).root

  def coverColumn(col: Column): Unit = {
//    col.r.l = col.l
//    col.l.r = col.r
//
//    var i = col.d
//    while(i != col) {
//      var j = i.r
//      while (j != i) {
//        j.d.u = j.u
//        j.u.d = j.d
//        j.c.asInstanceOf[Column].s -= 1
////        assert(j.c.asInstanceOf[Column].s >= 0)
//
//        j = j.r
//      }
//
//      i = i.d
//    }
  }

  def unconverColumn(col: Column): Unit = {
//    var i = col.u
//    while(i != col) {
//      var j = i.l
//      while(j != i) {
//        j.c.asInstanceOf[Column].s = j.asInstanceOf[Column].s + 1
//        j.d.u = j
//        j.u.d = j
//        j = j.l
//      }
//
//      i = i.u
//    }
//
//    col.r.l = col
//    col.l.r = col
  }

  /**
    * Choose column with the lowest number of 1s
    *
    * @return
    */
//  private def chooseColumn(): Column = {
//    var j = h.r
//    var col = j
//    var s = Int.MaxValue
//
//    while (j != h) {
//      if (j.asInstanceOf[Column].s < s) {
//        col = j
//        s = j.asInstanceOf[Column].s
//      }
//
//      j = j.r
//    }
//
//    col.asInstanceOf[Column]
//  }

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

//    val c = chooseColumn()
//
//    coverColumn(c)
//    var r = c.d
//    while (r != c) {
//      O += r
//      var j = r.r
//      while(j != r) {
//        coverColumn(j.c.asInstanceOf[Column])
//        j = j.r
//      }
//
//      search(O, sol)
//      j = r.l
//      while (j != r) {
//        unconverColumn(j.c.asInstanceOf[Column])
//        j = j.l
//      }
//
//      O -= r
//      r = r.d
//    }
//
//    unconverColumn(c)
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
