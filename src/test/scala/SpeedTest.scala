import org.scalatest.*
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class SpeedTest extends AnyFreeSpec with ParallelTestExecution {
	
	val timeLimit = 5.seconds
	implicit val ec: ExecutionContext = ExecutionContext.global
	for problem <- 1 to 100 do
		s"Problem $problem should be solved within $timeLimit seconds." in {
			val solution = Future { solve(problem) }
			Await.result(solution, timeLimit)
		}
	
	def solve(problem: Int): Unit =
		def unimplemented = intercept[TestFailedException] { // warning
			"Solution not yet written" should be("Solution written")
		}
		
		// TODO: Map each problem number to the correct test, preferably through reflection.
		val name = f"Problem$problem%03d"
		problem match {
			case 1 => Problem001
			case 2 => Problem002
			case 3 => Problem003
			case 4 => Problem004
			case 5 => Problem005
			case 6 => Problem006
			case 7 => Problem007
			case 8 => Problem008
			case 9 => Problem009
			case 10 => Problem010
			case 22 => Problem022
			case 23 => Problem023
			case 26 => Problem026
			case 27 => Problem027
			case 28 => Problem028
			case 33 => Problem033
			case 35 => Problem035
			case 36 => Problem036
			case 37 => Problem037
			case 38 => Problem038
			case 39 => Problem039
			case 40 => Problem040
			case 42 => Problem042
			case 43 => Problem043
			case 44 => Problem044
			case 45 => Problem045
			case 46 => Problem046
			case 47 => Problem047
			case 50 => Problem050
			case 51 => Problem051
			case 54 => Problem054
			case 55 => Problem055
			case 58 => Problem058
			case 60 => Problem060
			case _ => unimplemented
		}
}