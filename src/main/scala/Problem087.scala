/* Problem 87: Prime Power Triples
The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there
are exactly four numbers below fifty that can be expressed in such a way:
	28 = 2^2 + 2^3 + 2^4
	33 = 3^2 + 2^3 + 2^4
	49 = 5^2 + 2^3 + 2^4
	47 = 2^2 + 3^3 + 2^4

How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
*/
import math.{cbrt, pow, sqrt}

object Problem087 extends App {
	val primes =
		def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
		2 #:: (LazyList.range(3, Int.MaxValue, step = 2).filter(isPrime))
	
	val Limit = 50_000_000
	
	lazy val answer =
		val primePowerTriples = for {
			c <- primes.takeWhile(_ < sqrt(sqrt(Limit)))
			c4 = pow(c, 4).toInt
			b <- primes.takeWhile(_ < cbrt(Limit - c4))
			b3c4 = pow(b, 3).toInt + c4
			a <- primes.takeWhile(_ < sqrt(Limit - b3c4))
			a2b3c4 = pow(a, 2).toInt + b3c4
		} yield a2b3c4
		primePowerTriples.distinct.size
	
	println(answer)
}