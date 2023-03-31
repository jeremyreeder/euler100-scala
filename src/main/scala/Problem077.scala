/* Problem 77: Prime Summations
It is possible to write ten as the sum of primes in exactly five different ways:
	7 + 3
	5 + 5
	5 + 3 + 2
	3 + 3 + 2 + 2
	2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?
*/
import scala.annotation.tailrec

object Problem077 extends App {
	
	def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
	
	val primes = 2 #:: LazyList.range(3, Int.MaxValue, step = 2).filter(isPrime)
	
	/* Having observed that, for values of n from 4 to 12, primeSummations(n) increments once at each non-prime value of
	 * n and temporarily reverts to zero upon each prime value, I conjecture that this is true for all larger values of n
	 * as well. */
	def primeSummations(n: Int) =
		if n < 4 || isPrime(n) then 0
		else n - primes.takeWhile(_ < n).size - 1
	
	val Floor = 5000
	val answer = (LazyList from Floor).find(primeSummations(_) > Floor).get // This outputs 5759, but that's wrong.
	println(answer)
}