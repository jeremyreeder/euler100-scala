import Math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable

object Problem069 extends App {
	@tailrec
	private def greatestCommonDivisor(a: Int, b: Int): Int = // Euclidean algorithm
		if b == 0 then a else greatestCommonDivisor(b, a % b)
	
	def areCoprimes(a: Int, b: Int) =
		greatestCommonDivisor(a, b) == 1
	
	def phi(n: Int) = (1 until n).count(areCoprimes(n, _))
	
	def nToPhiRatio(n: Int) = n.toFloat / phi(n)
	
	val startTime = System.currentTimeMillis()
	val answer = (2 to 100_000) maxBy nToPhiRatio
	println(System.currentTimeMillis() - startTime)
	println(answer)
	
	//TODO: Make it faster. This has yet to complete, and it may never complete without a more efficient approach.
}
