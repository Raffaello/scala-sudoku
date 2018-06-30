package dlx.transform

import scala.collection.Set

/**
  * order here seems important, now tested only with Int type, need more testing for general types
  * On the other hand, no need more than int/byte types for this project (sudoku)
  *
  * @todo the value of the sets parameter should be of type 'universe' as a subtype of Set[T]
  */
object SetProblem {
  def convert[T](universe: Set[T], sets: Map[String, Set[T]]): Array[Array[Boolean]] = {
    val array = new Array[Array[Boolean]](sets.keys.size)
    val uSeq= universe.toIndexedSeq

    sets.keys.toList.sorted.zipWithIndex.foreach { case (k, row) =>
      val a:Array[Boolean] = Array.fill[Boolean](universe.size)(false)
      sets(k).foreach(x => if (universe.contains(x)) a(uSeq.indexOf(x, 0)) = true)
      array(row) = a
    }

    array
  }

  /**
    *
    * @param sets
    * @param solution
    * @tparam T
    * @return
    */
  def unconvert[T](sets: Map[String, Set[T]], solution: Set[Int]): Set[String] = {
    val orderedSets = sets.keys.toList.sorted

    solution.map(x => orderedSets(x))
  }
}
