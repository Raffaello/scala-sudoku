package dlx

import scala.annotation.tailrec

/**
  * Data Object
  *
  * @param l left,  pointer to the left of doubly circular linked list
  * @param r right, pointer to the right of doubly circular linked list
  * @param u up,    pointer to the above of doubly circular linked list
  * @param d down,  pointer to the below of doubly circular linked list
  * @param c        pointer to column list header
  */
class Data(
  var l: Data = null,
  var r: Data = null,
  var u: Data = null,
  var d: Data = null,
  var c: Column = null
)

object Data {
 @tailrec
 final def fold[T](acc: T, start: Data, cur: Data)(f: (T, Data) => (T, Data)): T = {
   if (start == cur) {
     acc
   }
   else {
    val (accumulator, current) = f(acc, cur)
    fold(accumulator, start, current)(f)
   }
 }
}
