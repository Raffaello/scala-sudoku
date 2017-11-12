package dlx

/**
  * Column Object
  *
  * @param s size,   number of 1s in the column
  * @param n nameId, column id number (index)
  * @param l left
  * @param r right
  * @param u up
  * @param d down
  * @param c list header
  */
class Column(
  var s: Int,
  var n: Int,
  l: Data = null,
  r: Data = null,
  u: Data = null,
  d: Data = null,
  c: Data = null
) extends Data(l, r, u, d, l)
