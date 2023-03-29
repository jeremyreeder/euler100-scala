/* Problem 66: Diophantine Equation
Consider quadratic Diophantine equations of the form:
	x^2 – D*y^2 = 1

For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
It can be assumed that there are no solutions in positive integers when D is square.
By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
	3^2 – 2×2^2 = 1
	2^2 – 3×1^2 = 1
	9^2 – 5×4^2 = 1
	5^2 – 6×2^2 = 1
	8^2 – 7×3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
*/
import math.sqrt
import System.currentTimeMillis

object Problem066 extends App {
	val Limit = 1000
	
	def chakravala(d: Int): (Int, Int) = {
		val m0 = math.sqrt(d).toInt
		var x = m0
		var y = 1
		var k = x * x - d * y * y
		
		while k.abs != 1  && y != 0 do
			val m = (m0 + x) / k
			val nextX = m * k - x
			val nextY = (d - nextX * nextX) / y
			val kNext = (nextX * nextX - d) / k
			x = nextX
			y = nextY.abs
			k = kNext
		(x, y)
	}
	
	val nonSquares =
		def isSquare(n: Long) = { val root = sqrt(n); root == root.floor }
		(2 to Limit).filterNot(isSquare)
	
	val answer = nonSquares.maxBy(chakravala(_)._1) // 962? Wrong.
	
	val startTime = currentTimeMillis()
	println(answer)
	println(s"That took ${currentTimeMillis() - startTime} milliseconds to calculate.")
}