package dlx.transform

import scala.collection.Set

/**
  * order here seems important, now tested only with Int type, need more testing for general types
  * On the other hand, no need more than int/byte types for this project (sudoku)
  */
object SetProblem {
  def convert[T](universe: Set[T], sets: Map[String, Set[T]]): Array[Array[Boolean]] = {
    var array = Array[Array[Boolean]]()
    val uSeq= universe.toIndexedSeq

    sets.keys.toList.sorted.zipWithIndex.foreach { case (k, row) =>
      var a:Array[Boolean] = Array.fill[Boolean](universe.size)(false)
      sets(k).foreach(x => if (universe.contains(x)) a(uSeq.indexOf(x)) = true)
      array :+= a
    }

    array
  }
}
