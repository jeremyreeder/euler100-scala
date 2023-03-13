/* Problem 3: Largest prime factor
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*/

import Math.sqrt

object Problem003 extends App {
	val primes = 2L #:: 3L #:: {
		LazyList range(5L, sqrt(Long.MaxValue).toLong, 2L)
	} filter { n => (2L to sqrt(n).toLong) forall (n % _ != 0) }
	
	private def largestPrimeFactor(n: Long) =
		var x = n
		var result = 1L
		while x > result do
			for y <- primes takeWhile (_ <= x) do
				if x % y == 0 then
					x /= y
					result = y
		result
	
	println(s"The first ten prime numbers are: ${(primes take 10).toList mkString ", "}.")
	for number <- Seq(13_195, 600_851_475_143L) do
		println(s"The largest prime factor of $number is ${largestPrimeFactor(number)}.")
}
