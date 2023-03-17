/* Problem 60: Prime Pair Sets
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the
result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes,
792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
*/
import collection.mutable.ArrayBuffer

object Problem060 extends App {

	def isPrime(n: Int) = BigInt(n).isProbablePrime(certainty = 8)
	
	def concat(a: Int, b: Int): Int = (a.toString + b.toString).toInt
	
	def isPairPrime(a: Int, b: Int): Boolean = isPrime(concat(a, b)) && isPrime(concat(b, a))
	
	def findPrimeSets(primes: Iterable[Int]): List[ArrayBuffer[Int]] = {
		val result = ArrayBuffer[ArrayBuffer[Int]]()
		
		def search(depth: Int, accumulator: ArrayBuffer[Int]): Unit =
			if depth == 5 then result += accumulator.clone()
			else for p <- primes if accumulator.forall(q => p < q && isPairPrime(p, q)) do
					accumulator += p
					search(depth + 1, accumulator)
					accumulator.dropRightInPlace(1)
		
		for p <- primes do search(1, ArrayBuffer(p))
		result.toList
	}
	
	// Generate list of prime numbers
	val primes = 2 #:: 3 #:: LazyList.range(5, 8_400, 2).filter(isPrime)
	
	// Find sets of five primes that satisfy the conditions
	val primeSets = findPrimeSets(primes)
	
	// Find the set with the smallest sum
	val answer = primeSets.map(_.sum).min
	println(answer)
	
	//TODO: Make it faster. This takes about 28 seconds to complete.
}