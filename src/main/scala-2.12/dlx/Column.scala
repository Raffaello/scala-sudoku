package dlx

/**
  * Column Object
  *
  * @param s size,   number of 1s in the column
  * @param n nameId, column id number (index)
  */
final class Column(
  var s: Int,
  var n: Int,
  l: Column = null,
  r: Column = null,
  u: Data = null,
  d: Data = null,
  c: Column = null,
) extends Data(l,r,u,d, c)
