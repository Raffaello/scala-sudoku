package DLX

import dlx.{Column, SparseMatrix, Data}
import org.scalatest.{FlatSpec, Matchers}

class SparseMatrixSpec extends FlatSpec with Matchers {
  "dlx.Matrix" should "throw IllegalArgumentException when input is not a m*n Matrix" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](true, true),
        Array[Boolean](true, true, true)
      ))
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero rows" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array())
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero cols" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(Array()))
    }
  }

  /** @deprecated */
  "dlx.Matrix" should "have the same size of the input (deprecated)" in {
    val matrix = new SparseMatrix(Array(
      Array[Boolean](true, true, true),
      Array[Boolean](true, true, true),
      Array[Boolean](true, true, true)
    ))
    val ptr = matrix.root.r
    var myPtr = ptr
    var count = 0

    while (myPtr.r != ptr) {
      count += 1
      myPtr = myPtr.r
    }

    count should be(3)

    myPtr = ptr
    count = 0
//    while (myPtr.d != ptr) {
//      count += 1
//      myPtr = myPtr.d
//    }

    count should be(3)
  }


  "dlx.Matrix" should "have the same size of the input" in {
    var matrix = new SparseMatrix(Array(
      Array[Boolean](true, true, true)
    ))

    var ptr = matrix.root
    var count = 1
//    var myPtr:Data = ptr
//
//    while (myPtr.r != ptr) {
//      count += 1
//      myPtr = myPtr.r
//      myPtr.d should be(myPtr.u)
//    }

    //header size
    count should be(3)




//    matrix = new Matrix(Array(
//      Array[Int](1, 1, 1),
//      Array[Int](1, 1, 1),
//      Array[Int](1, 1, 1)
//    ))

//    ptr = matrix.root.asInstanceOf[Data]
////    mat = matrix.mat
//     myPtr = ptr
//     count = 1
//
//    while (myPtr.r != ptr) {
//      count += 1
//      myPtr = myPtr.r
//    }
//
//    count should be(3)
//
//    myPtr = ptr
//    count = 1
//    while (myPtr.d != ptr) {
//      count += 1
//      myPtr = myPtr.d
//    }
//
//    count should be(3)
  }

  "dlx.Matrix" should "have the correct header column values" in {
    val matrix = new SparseMatrix(Array(
      Array[Boolean](true, true, true),
      Array[Boolean](false, true, true),
      Array[Boolean](false, false, true)
    ))

    val ptr = matrix.root
    ptr.s should be(1)
//    ptr.c.asInstanceOf[Column] should be equals(ptr)
    ptr.r.asInstanceOf[Column].s should be(2)
    ptr.r.r.asInstanceOf[Column].s should be(3)
  }
}
