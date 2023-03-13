/* Problem 27: Quadratic Primes
Euler discovered the remarkable quadratic formula:
  n^2 + n + 41
It turns out that the formula will produce 40 primes for the consecutive integer values 0 <= n <= 39. However, when
n = 40, 40^2 + 40 + 41 = 40 (40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly
divisible by 41.
The incredible formula n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values
0 <= n <= 79. The product of the coefficients, −79 and 1601, is −126479.
Considering quadratics of the form:
  n^2 + a*n + b, where |a| < 1000 and |b| <= 1000
  where |n| is the modulus/absolute value of n
  e.g. |11| is 11 and |-4| is 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes
for consecutive values of n, starting with n = 0.
*/

import Math.pow

object Problem027 extends App {
	private def consecutivePrimeCount(a: Int, b: Int) =
		def quadratic(a: Int, b: Int, n: Int) = BigInt(pow(n, 2).toLong) + BigInt(a) * n + b
		
		(LazyList from 0).takeWhile(quadratic(a, b, _).isProbablePrime(1)).length
	
	private val combinations = (-999 to 999).flatMap(a => (-1000 to 1000).map(b => (a, b)))
	val answer = combinations.maxBy(consecutivePrimeCount).toList.product
	println(answer)
}
