/* Problem 77: Prime Summations
It is possible to write ten as the sum of primes in exactly five different ways:
	7 + 3
	5 + 5
	5 + 3 + 2
	3 + 3 + 2 + 2
	2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?
*/
import annotation.tailrec

object Problem077 extends App {
	
	def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
	
	def findNumberWithSummationCountGreaterThan(floor: Int): Int = {
		def search(floor: Int, limit: Int): Option[Int] =
			val partitions = Array.fill(limit + 1)(0)
			partitions(0) = 1
			for {
				i <- partitions.indices
					if isPrime(i)
				j <- i to partitions.indices.last
			} do partitions(j) += partitions(j - i)
			partitions.find(_ > floor)
		
		var limit = 1
		while limit > 0 do
			search(floor, limit) match {
				case Some(n) => return n
				case None => limit *= 2
			}
		throw Error("No answer found.")
	}
	
	val answer = findNumberWithSummationCountGreaterThan(floor = 10) // Buggy. Says 12, but the real answer is 5.
	println(answer)
}