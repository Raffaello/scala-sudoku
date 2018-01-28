import dlx.DLX
import dlx.transform.{NQueensProblem, Solution1}
import org.scalatest.{FlatSpec, Matchers}

class NQueensSolverSpec extends FlatSpec with Matchers {
  "Solution 1" should "solved correctly" in {
    val dlx = new DLX(NQueensProblem.convert(Solution1.checkBoard))
    val sols = dlx.solve()
    sols should have length 1
    val sol = sols.head
    sol should have size 8
    for (s <- sol) {
      s should have length 4
      for (i <- 0 until 4) {
        val ss = s.sorted
        //ss(i) shuld be ()
      }
    }

    NQueensProblem.unconvert(sol) should be (Solution1.checkboardSol)
  }
}
