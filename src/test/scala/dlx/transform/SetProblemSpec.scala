package dlx.transform

import dlx.PaperProblem
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.SortedSet

final class SetProblemSpec extends FlatSpec with Matchers {

  object PaperSetProblem {
    val universe = SortedSet(1, 2, 3, 4, 5, 6, 7)
    val paperProblem = Map[String, Set[Int]](
      "A" -> Set(3, 5, 6),
      "B" -> Set(1, 4, 7),
      "C" -> Set(2, 3, 6),
      "D" -> Set(1, 4),
      "E" -> Set(2, 7),
      "F" -> Set(4, 5, 7),
      )
  }

  private def checker(input: Array[Array[Boolean]], expected: Array[Array[Boolean]]): Unit = {
    input.length should be(expected.length)
    for(i <- input.indices) {
      input(i).length should be (expected(i).length)
      for(j <- input(i).indices) {
        input(i)(j) should be (expected(i)(j))
      }
    }
  }

  "Paper set Problem" should "be transformed properly" in {
    val p = SetProblem.convert[Int](PaperSetProblem.universe, PaperSetProblem.paperProblem)
    checker(p, PaperProblem.matrix)
  }

  "Paper solution" should "be reverted properly" in {
    SetProblem.unconvert(PaperSetProblem.paperProblem, Set(0, 3, 4)) should be(Set("A", "D", "E"))
  }

  "Wikipedia set problem" should "be transformed properly" in {
    val u = SortedSet(1, 2, 3, 4, 5, 6, 7)
    val sets = Map[String, Set[Int]](
      "A" -> Set(1, 4, 7),
      "B" -> Set(1, 4),
      "C" -> Set(4, 5, 7),
      "D" -> Set(3, 5, 6),
      "E" -> Set(2, 3, 6, 7),
      "F" -> Set(2, 7)
    )
    val matrix = Array(
      //     1       2      3      4      5      6      7
      Array(true,  false, false, true,  false, false, true),  // A
      Array(true,  false, false, true,  false, false, false), // B
      Array(false, false, false, true,  true,  false, true),  // C
      Array(false, false, true,  false, true,  true,  false), // D
      Array(false, true,  true,  false, false, true,  true),  // E
      Array(false, true,  false, false, false, false, true),  // F
    )

    checker(SetProblem.convert[Int](u, sets), matrix)
  }

  "Thesis set Problem" should "be transformed properly" in {
    val u = SortedSet[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val sets = Map[String, Set[Byte]](
      "A" -> Set(1, 2, 5, 7),
      "B" -> Set(3, 4, 6),
      "C" -> Set(1, 5, 9),
      "D" -> Set(1, 3, 4, 5, 6, 8, 9),
      "E" -> Set(2, 7, 8)
    )
    val matrix = Array(
      //      1      2      3      4      5      6      7      8      9
      Array(true,  true,  false, false, true,  false, true,  false, false), // A
      Array(false, false, true,  true,  false, true,  false, false, false), // B
      Array(true,  false, false, false, true,  false, false, false, true), // C
      Array(true,  false, true,  true,  true,  true,  false, true,  true), // D
      Array(false, true,  false, false, false, false, true,  true,  false), // E
    )

    checker(SetProblem.convert[Byte](u, sets), matrix)
  }
}
