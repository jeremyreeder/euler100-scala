/* Problem 46: Goldbach's Other Conjecture
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a
square.
  9 = 7 + 2×12
  15 = 7 + 2×22
  21 = 3 + 2×32
  25 = 7 + 2×32
  27 = 19 + 2×22
  33 = 31 + 2×12
It turns out that the conjecture was false.
What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
*/
import Math.pow
object Problem046 extends App {
  def isPrime(n: Int) = BigInt(n).isProbablePrime(1)
  val primes = 2 #:: LazyList.range(3, Int.MaxValue, 2).filter(isPrime)
  val oddComposites = LazyList.range(9, Int.MaxValue, 2).filterNot(isPrime)
  val doubleSquares = (LazyList from 1).map(pow(_, 2).toInt * 2)
  def isGoldbach(oddComposite: Int): Boolean =
    for prime <- primes.takeWhile(_ < oddComposite)
        doubleSquare = doubleSquares.dropWhile(_ < oddComposite - prime).head
        if oddComposite == prime + doubleSquare
      do return true
    false
  val answer = oddComposites.filterNot(isGoldbach).head
  println(answer)
}
