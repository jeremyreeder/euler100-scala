import util.control.Breaks.{break, breakable}

object Problem069 extends App {
	
	val primes =  {
		def isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
		
		2 #:: LazyList.range(3, Int.MaxValue, step = 2).filter(isPrime)
	}
	
	// The ratio of n to phi(n) peaks ever-increasingly each time n reaches the product of a series of the smallest
	// primes possible, so the answer is the product of the longest series of primes whose product doesn't exceed the
	// limit, as follows.
	
	var answer = primes.head
	var product = answer
	breakable {
		for n <- primes.tail do
			product *= n
			if product > 1_000_000 then break
			answer = product
	}
	println(answer)
}