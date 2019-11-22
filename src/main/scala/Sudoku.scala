import dlx.DLX
import dlx.transform.SudokuProblem

object Sudoku /*extends App*/ {
  val grid = Array(
    Array[Byte](0, 7, 0, 2, 3, 8, 0, 0, 0),
    Array[Byte](0, 0, 0, 7, 4, 0, 8, 0, 9),
    Array[Byte](0, 6, 8, 1, 0, 9, 0, 0, 2),
    Array[Byte](0, 3, 5, 4, 0, 0, 0, 0, 8),
    Array[Byte](6, 0, 7, 8, 0, 2, 5, 0, 1),
    Array[Byte](8, 0, 0, 0, 0, 5, 7, 6, 0),
    Array[Byte](2, 0, 0, 6, 0, 3, 1, 9, 0),
    Array[Byte](7, 0, 9, 0, 2, 1, 0, 0, 0),
    Array[Byte](0, 0, 0, 9, 7, 4, 0, 8, 0)
  )

  val start = System.currentTimeMillis()
  val dlx = new DLX(SudokuProblem.convert(grid))
  val startSolver = System.currentTimeMillis()
  val sols = dlx.solve()
  val endSolver = System.currentTimeMillis()

  sols.foreach(s => {
    SudokuProblem.unconvert(s).foreach(r => println(r.mkString(" ")))

  })

  val end = System.currentTimeMillis()
  println(s"Solver time: ${endSolver-startSolver}ms")
  println(s"Total time: ${end-start}ms")
}
