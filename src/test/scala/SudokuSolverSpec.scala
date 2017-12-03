import dlx.DLX
import dlx.transform.{SudokuExample1, SudokuProblem}
import org.scalatest.{FlatSpec, Matchers}

final class SudokuSolverSpec extends FlatSpec with Matchers {

  "Sudoku Example1" should "be solved correctly" in {

    val dlx = new DLX(SudokuProblem.convert(SudokuExample1.grid))
    val sols = dlx.solve()
    sols.length should be (1)
    val sol = sols.head
    SudokuProblem.unconvert(sol) should be (SudokuExample1.sol)
  }
}
