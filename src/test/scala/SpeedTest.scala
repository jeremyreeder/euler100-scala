import org.scalatest.*
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class SpeedTest extends AnyFreeSpec with ParallelTestExecution {
	
	private val TimeLimit = 1.minutes
	implicit val ec: ExecutionContext = ExecutionContext.global
	for problem <- 1 to 100 do
		s"Problem $problem should be solved within $TimeLimit." in {
			val solution = Future { solve(problem) }
			Await.result(solution, TimeLimit)
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
			case 57 => Problem057
			case 58 => Problem058
			case 59 => Problem059
			case 60 => Problem060
			case 61 => Problem061
			case 62 => Problem062
			case 63 => Problem063
			case 66 => Problem066
			case 69 => Problem069
			case 70 => Problem070
			case 71 => Problem071
			case 72 => Problem072
			case 73 => Problem073
			case 74 => Problem074
			case 75 => Problem075
			case 76 => Problem076
			case 77 => Problem077
			case 78 => Problem078
			case 79 => Problem079
			case 80 => Problem080
			case 81 => Problem081
			case 82 => Problem082
			case 83 => Problem083
			case 85 => Problem085
			case 87 => Problem087
			case 88 => Problem088
			case 89 => Problem089
			case 90 => Problem090
			case 91 => Problem091
			case 94 => Problem094
			case 97 => Problem097
			case 99 => Problem099
			case _ => unimplemented
		}
}
