package DLX

import dlx.{Column, DLX}
import org.scalatest.{FlatSpec, Matchers}

class DLXSpec extends FlatSpec with Matchers {

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


  "DLX" should "have the same size of the input" in {
    val input = Array(
      Array[Int](1, 1, 1),
      Array[Int](1, 1, 1),
      Array[Int](1, 1, 1)
    )
    val dlx = new DLX(input)
    val ptr = dlx.h.r
    var myPtr = ptr
    var count = 0

    while (myPtr.r != ptr) {
      count += 1
      myPtr = myPtr.r
    }

    count should be(3)

    myPtr = ptr
    count = 0
    while (myPtr.d != ptr) {
      count += 1
      myPtr = myPtr.d
    }

    count should be(3)
  }




}
