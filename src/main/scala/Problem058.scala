/* Problem 58: Spiral Primes
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
	37 36 35 34 33 32 31
	38 17 16 15 14 13 30
	39 18  5  4  3 12 29
	40 19  6  1  2 11 28
	41 20  7  8  9 10 27
	42 21 22 23 24 25 26
	43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that
8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this
process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals
first falls below 10%?
*/

import Math.{pow, sqrt}

object Problem058 extends App {
	
	def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
	
	def answer: Int =
		var (diagonals, primeDiagonals) = (1, 0) // A square of length 1 has only 1 corner, and it's not prime.
		for {
			length <- LazyList.range(3, sqrt(Int.MaxValue).toInt, step = 2)
		} do {
			diagonals += 4
			val bottomRight = pow(length, 2).toInt
			val primeCornerCount = (1 to 3).map(bottomRight - _ * (length - 1)).count(isPrime)
			primeDiagonals += primeCornerCount
			if diagonals > 10 * primeDiagonals then
				return length
		}
		throw Error("No answer found.")
	
	println(answer)
}