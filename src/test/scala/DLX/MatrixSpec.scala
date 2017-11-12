package DLX

import dlx.Matrix
import org.scalatest.{FlatSpec, Matchers}

class MatrixSpec extends FlatSpec with Matchers {
  "dlx.Matrix" should "throw IllegalArgumentException when input is not a m*n Matrix" in {
    intercept[IllegalArgumentException] {
      new Matrix(Array(
        Array[Int](1, 1),
        Array[Int](1, 1, 1)
      ))
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero rows" in {
    intercept[IllegalArgumentException] {
      new Matrix(Array())
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero cols" in {
    intercept[IllegalArgumentException] {
      new Matrix(Array(Array()))
    }
  }

  "dlx.Matrix" should "have the same size of the input" in {
    val matrix = new Matrix(Array(
      Array[Int](1, 1, 1),
      Array[Int](1, 1, 1),
      Array[Int](1, 1, 1)
    ))
    val ptr = matrix.header.r
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
