import dlx.DLX
import dlx.transform.{SudokuExample1, SudokuProblem}
import org.scalatest.{FlatSpec, Matchers}

final class SudokuSolverSpec extends FlatSpec with Matchers {

  def checkSol(grid: Array[Array[Byte]], exp: Array[Array[Byte]]) {
    val dlx = new DLX(SudokuProblem.convert(grid))
    val sols = dlx.solve()
    sols should have length 1
    val sol = sols.head
    sol should have size 81
    for (s <- sol) {
      s should have length 4
      for(i <- 0 until 4) {
        val ss = s.sorted
        ss(i) should (be >= 81*i and be < 81*(i+1))
      }
    }
    SudokuProblem.unconvert(sol) should be (exp)
  }

  "Sudoku Example1 solution"  should "be solved correctly" in {
    checkSol(SudokuExample1.sol, SudokuExample1.sol)
  }

  "Sudoku Example1" should "be solved correctly" in {
    checkSol(SudokuExample1.grid, SudokuExample1.sol)
  }
}
