import Math.pow

object Problem006 extends App {
	private def sumSquareDifference(numbers: Iterable[Int]) =
		val sumOfSquares = numbers.map(pow(_, 2).toInt).sum
		val squareOfSum = pow(numbers.sum, 2).toInt
		squareOfSum - sumOfSquares
	
	Seq(1 to 10, 1 to 100) map sumSquareDifference foreach println
}
