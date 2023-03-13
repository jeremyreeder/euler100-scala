/* Problem 47: Distinct Prime Factors
The first two consecutive numbers to have two distinct prime factors are:
	14 = 2 × 7
	15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:
	644 = 2² × 7 × 23
	645 = 3 × 5 × 43
	646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
*/

import Math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable
import System.currentTimeMillis

object Problem047 extends App {
	def isPrime(n: Int) = BigInt(n).isProbablePrime(certainty = 1)
	
	val primes = 2 #:: 3 #:: LazyList.range(5, Int.MaxValue, step = 2).filter(isPrime)
	
	def memoize[I, O](f: I => O): I => O =
		new mutable.HashMap[I, O]() {
			override def apply(key: I) = getOrElseUpdate(key, f(key))
		}
	
	val distinctFactorCount: Int => Int = memoize(n => {
		n match {
			case n if n < 2 => 0
			case _ =>
				@tailrec
				def loop(n: Int, accumulator: Int): Int =
					n match {
						case 1 => accumulator
						case n if isPrime(n) => accumulator + 1
						case n =>
							val p = primes.takeWhile(_ <= sqrt(n)).find(n % _ == 0).get
							var q = n / p
							while q % p == 0 do
								q /= p
							loop(q, accumulator + 1)
					}
				
				loop(n, accumulator = 0)
		}
	})
	
	def startsChain(n: Int, length: Int, f: Int => Boolean) =
		!(n until n + length).map(f).contains(false)
	
	for i <- 2 to 4 do
		val startTime = currentTimeMillis()
		val answer = (LazyList from 1).find(n => startsChain(n, i, distinctFactorCount(_) == i)).get
		val elapsedTime = currentTimeMillis() - startTime
		println(s"The first chain of $i consecutive integers to have $i distinct prime factors starts at $answer.")
		println(s"It took $elapsedTime milliseconds to determine this.")
		println()
}