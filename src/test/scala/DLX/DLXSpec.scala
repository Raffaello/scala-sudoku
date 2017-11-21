package DLX

import dlx.{Column, DLX}
import org.scalatest.{FlatSpec, Matchers}

class DLXSpec extends FlatSpec with Matchers {

  "DLX.chooseColumn" should "be chosen correctly" in {
    val dlx = new DLX(Array(
      Array[Boolean](true)
    ))
  }

//  "The Exact Cover problem" should "be solved correctly" in {
//    val u: Array[Int] = Range(1,7).toArray
//    val sets: Map[String, Array[Int]] = Map[String, Array[Int]](
//      "A" -> Array(3, 5, 6, 8),
//      "B" -> Array(1, 4, 7),
//      "C" -> Array(2, 3, 6),
//      "D" -> Array(1, 4),
//      "E" -> Array(2, 7),
//      "F" -> Array(4, 5, 7),
//      "G" -> Array(8, 9),
//      "H" -> Array(3, 5, 6)
//    )
//  }

  "DLX paper exact cover problem example" should "be solved correctly" in {
//    val dlx = new DLX(Array(
//      Array(0, 0, 1, 0, 1, 1, 0),
//      Array(1, 0, 0, 1, 0, 0, 1),
//      Array(0, 1, 1, 0, 0, 1, 0),
//      Array(1, 0, 0, 1, 0, 0, 0),
//      Array(0, 1, 0, 0, 0, 0, 1),
//      Array(0, 0, 0, 1, 1, 0, 1)
//    ))
//
//    dlx.solve() should be(Array(1))

    val dlx = new DLX(PaperProblem.matrix)
    val result = dlx.solve()
    println(result)
  }
}
