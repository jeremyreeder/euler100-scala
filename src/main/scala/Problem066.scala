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
import math.Integral.Implicits.*
import math.sqrt
import scala.collection.mutable
import System.currentTimeMillis

object Problem066 extends App {
	val Limit = 1000
	
	val PositiveSquares =
		(LazyList from 1).map({ n =>
			var nLong = n.toLong
			nLong * nLong
		}).takeWhile(_ >= 0)
	
	val Squares =
		(LazyList from 0).map({ n =>
			var nLong = n.toLong
			nLong * nLong
		}).takeWhile(_ >= 0)
	
	def isSquare(n: Int) =
		val root = sqrt(n)
		root == root.floor
	
	def isSquare(n: Long) =
		val root = sqrt(n)
		root == root.floor
	
	def minimalX(d: Long): Long =
		if isSquare(d) then return 0
		for {
			y2 <- Squares.drop(1)
			x2 = d * y2 + 1
			if x2 > 0 && isSquare(x2)// && x2 - d * y2 == 1
		} do return sqrt(x2).toLong
		throw Error(s"No x found for d=$d.")
	
	def answer: Long = // TODO: Still way too slow.
		var ds = (2 to Limit).map(_.toLong).filterNot(isSquare)
		for {
			x2 <- PositiveSquares
			y2 <- PositiveSquares.takeWhile(_ < x2)
			(d, r) = (x2 - 1) /% y2
			if r == 0 && (ds contains d)
		} do
			ds = ds.filter(_ != d)
			println(s"x^2 = $x2; y^2 = $y; ${ds.size} possible values of D.")
			if ds.size < 20 then
				println(s"Possible values of D are: ${ds.mkString(", ")}.")
			if ds.size == 1 then return ds.head
		throw Error("No answer found.")
	
	val startTime = currentTimeMillis()
	println(answer)
	println(s"That took ${(currentTimeMillis() - startTime) / 1000} seconds to calculate.")
}
