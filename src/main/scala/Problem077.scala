/* Problem 77: Prime Summations
It is possible to write ten as the sum of primes in exactly five different ways:
	7 + 3
	5 + 5
	5 + 3 + 2
	3 + 3 + 2 + 2
	2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?
*/
object Problem077 extends App {
	
	val primes =
		def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
		2 #:: LazyList.range(3, Int.MaxValue, step = 2).filter(isPrime)
	
	def summationCounts(n: Int): Long = {
		val counts = Array.fill(n + 1)(0L)
		counts(0) = 1L
		for {
			p <- primes.takeWhile(_ <= n)
			i <- 0 to (n - p)
		} do counts(i + p) += counts(i)
		counts(n)
	}
	
	val answer = (LazyList from 1).find(summationCounts(_) > 5000).get
	println(answer)
}