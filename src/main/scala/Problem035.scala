/* Problem 35: Circular Primes
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves
prime. There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
How many circular primes are there below one million?
*/
object Problem035 extends App {
  def isPrime(n: Int) = BigInt(n).isProbablePrime(3)
  val smallPrimes = 2 #:: LazyList.range(3, Int.MaxValue, 2) filter isPrime takeWhile (_ < 1_000_000)
  def isCircular(prime: Int): Boolean =
    val s = prime.toString
    for rotation <- 1 to s.length - 1
        rotated = ((s drop rotation).take(s.length - rotation) + s.take(rotation)).toInt
      do if !isPrime(rotated) then return false
    true
  val answer = (smallPrimes filter isCircular).size
  println(answer)
}
