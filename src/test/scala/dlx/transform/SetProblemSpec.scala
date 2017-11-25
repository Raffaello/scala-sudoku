package dlx.transform

import dlx.PaperProblem
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.SortedSet

class SetProblemSpec extends FlatSpec with Matchers {
  "Paper set Problem" should "be transformed properly" in {
    val universe = SortedSet(1, 2, 3, 4, 5, 6, 7)
    val paperProblem = Map[String, Set[Int]](
      "A" -> Set(3, 5, 6),
      "B" -> Set(1, 4, 7),
      "C" -> Set(2, 3, 6),
      "D" -> Set(1, 4),
      "E" -> Set(2, 7),
      "F" -> Set(4, 5, 7),
    )

    val p = SetProblem.convert[Int](universe, paperProblem)
    p.length should be(PaperProblem.sparseMatrix.m)
    for(i <- p.indices) {
      p(i).length should be (PaperProblem.sparseMatrix.n)
      for(j <- p(i).indices) {
        p(i)(j) should be (PaperProblem.sparseMatrix.matrix(i)(j))
      }
    }
  }
}
