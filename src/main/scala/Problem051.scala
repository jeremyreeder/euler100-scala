/* Problem 51: Prime Digit Replacements
By replacing the 1st digit of the 2-digit number _3, it turns out that six of the nine possible values:
	13, 23, 43, 53, 73, and 83,
are all prime.

By replacing the 3rd and 4th digits of 56__3 with the same digit, this 5-digit number is the first example having seven
primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit,
is part of an eight prime value family.
*/
object Problem051 extends App {
	val primes =
		// The minimum certainty that gives the correct number of primes below a million is 8.
		def _isPrime(n: BigInt) = n.isProbablePrime(certainty = 8)
		2 :: 3 :: (5 until 1_000_000 by 2).filter(_isPrime).toList
	
	val isPrime = primes.toSet
	
	def familySize(prime: Int, blankCount: Int) =
		val blankBitMasks = (Seq.fill(blankCount)(true) ++ Seq.fill(prime.toString.length - blankCount)(false)).permutations
		val family =
			var result = 0
			for {
				subtitute <- 0 to 9
				position <- 0 to blankCount
			} do {}
			
	def substitute(string: String, substitution: Char, b: Seq[Boolean]) =
		val sb = StringBuilder()
		for pos <- 0 until string.length do
			if b(pos) then sb.append(substitution) else sb.append(string.charAt(pos))
		sb.toString()
		
	
	def answer: Int =
		val desiredFamilySize = 8
		for {
			prime <- primes
			primeString = prime.toString
			blankCount <- 1 until primeString.length
		} do {
			val (trues, falses) = (Seq.fill(blankCount)(true), Seq.fill(primeString.length - blankCount)(false))
			for blankMask <- (trues ++ falses).permutations do
				val family = ('0' to '9').map(substitute(primeString, _, blankMask))
					.filterNot(_.startsWith("0")).map(_.toInt).filter(isPrime)
				if family.size == desiredFamilySize then return family.head
		}
		0
	
	println(answer)
}