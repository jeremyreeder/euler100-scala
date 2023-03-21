import Math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable
import scalaz.Memo.mutableHashMapMemo

object Problem069 extends App {
	
	val Limit = 1_000_000
	
	def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
	
	val primes = 2 :: (3 to Limit by 2).filter(isPrime).toList
	
	val primeFactors: Int => List[Int] = mutableHashMapMemo { n =>
		@tailrec
		def loop(n: Int, accumulator: List[Int]): List[Int] =
			if isPrime(n) then return n :: accumulator
			if n < 2 then return accumulator
			primes.takeWhile(_ <= sqrt(n)).find(n % _ == 0) match {
				case Some(p) => loop(n / p, p :: accumulator)
				case None => accumulator
			}
		
		loop(n, List.empty)
	}
 
	def areCoprime(a: Int, b: Int) = {
		@tailrec
		def greatestCommonDivisor(a: Int, b: Int): Int =
			if b == 0 then a
			else greatestCommonDivisor(b, a % b)
		
		greatestCommonDivisor(a, b) == 1
	}
	
	val phi: Int => Int = mutableHashMapMemo {
		case n if n < 2 => 0
		case n if isPrime(n) => n - 1
		case n =>
			val factors = primeFactors(n)
			if factors.distinct.size == factors.size then
				factors.map(phi(_)).product
			else (1 until n).count(areCoprime(_, n))
	}
	
	def nToPhiRatio(n: Int) = n.toFloat / phi(n)
	
	// The value of phi(a*b) is minimized when a and b are prime. Additional factors can be included, e.g. phi(a*b*c).
	// Let's consider only values of n which are the product of two or more distinct primes. Whenever there are
	// duplicate prime factors, n does becomes larger but phi becomes proportionately larger as well, so there's no
	// increase in the resulting ratio.
	val candidates = (2 to Limit).map(primeFactors(_).distinct.product).distinct
	
	val answer = candidates.maxBy(nToPhiRatio)
	println(answer)
}