import dlx.DLX
import dlx.transform.{SudokuExample1, SudokuProblem}
import org.scalatest.tagobjects.Slow
import org.scalatest.{FlatSpec, Matchers}

final class SudokuSolverSpec extends FlatSpec with Matchers {

  /*"Sudoku Example1 solution"*/ ignore should "be ??? correctly" taggedAs Slow in {
    val dlx = new DLX(SudokuProblem.convert(SudokuExample1.sol))
    val sols = dlx.solve()
    sols.length should be (1)
    val sol = sols.head
    SudokuProblem.unconvert(sol) should be (SudokuExample1.sol)
  }

  /*"Sudoku Example1"*/ ignore should "be solved correctly" taggedAs Slow in {

    val dlx = new DLX(SudokuProblem.convert(SudokuExample1.grid))
    val sols = dlx.solve()
    sols.length should be (1)
    val sol = sols.head
    SudokuProblem.unconvert(sol) should be (SudokuExample1.sol)
  }
}
