/* Problem 7: 10001st Prime
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10001st prime number?
*/

import Math.sqrt

object Problem007 extends App {
	val primes = 2L #:: 3L #:: {
		LazyList range(5L, sqrt(Long.MaxValue).toLong, 2L)
	} filter { n => (2L to sqrt(n).toLong) forall (n % _ != 0) }
	Seq(6, 10_001) foreach { n => println(s"The ${n}th prime is ${primes(n - 1)}.") }
}
