/* Problem 10: Summation of Primes
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
*/

import System.currentTimeMillis
import Math.{pow, sqrt}

object Problem010 extends App {
	val primes = 2L #:: 3L #:: {
		LazyList range(5L, sqrt(Long.MaxValue).toLong, 2L)
	} filter { n => (2L to sqrt(n).toLong) forall (n % _ != 0) }
	val answer = primes.takeWhile(_ < 2_000_000L).sum
	println(answer)
}
