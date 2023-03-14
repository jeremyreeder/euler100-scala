import scala.collection.mutable

/* Problem 50: Consecutive Prime Sum
The prime 41, can be written as the sum of six consecutive primes:
	41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.
The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/
import util.control.Breaks._
object Problem050 extends App {
	def isPrime(n: BigInt) = n.isProbablePrime(certainty = 3)
	
	val primes = 2 :: 3 :: (5 until 1_000_000 by 2).filter(isPrime).toList
	
	val maxChainLength =
		var sum = 0
		primes.takeWhile(n => { sum += n; sum < primes.last }).size
	
	def answer: Int =
		for length <- maxChainLength to 2 by -1 do breakable {
			var testSum = 0
			val firstChain = primes.take(length).takeWhile(n => { testSum += n ; testSum < primes.last })
			if firstChain.size != length then break
			var rollingSum = firstChain.sum
			var start = 0
			while (start + length < primes.size) do
				if rollingSum >= primes.last then break
				if isPrime(rollingSum) then
					return rollingSum
				rollingSum -= primes(start)
				rollingSum += primes(start + length)
				start += 1
		}
		0
	
	println(answer)
}