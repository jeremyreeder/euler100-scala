/* Problem 37: Truncatable Primes
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left
to right, and remain prime at each stage: 3797, 797, 97, and 7.
Similarly we can work from right to left: 3797, 379, 37, and 3.
Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*/
import Math.{pow, sqrt}
import scala.collection.mutable
import System.currentTimeMillis
object Problem037 extends App {
  def isPrime(n: BigInt) = n.isProbablePrime(3)

  def primes = 2 #:: 3 #:: LazyList.range(5, Int.MaxValue, 2).filter(isPrime)

  def memoize[I, O](f: I => O): I => O =
    new mutable.HashMap[I, O]() {
      override def apply(key: I) = getOrElseUpdate(key, f(key))
    }

  val primesByLength: Int => LazyList[Int] = memoize {
    length =>
      primes.dropWhile(_ < pow(10, length - 1)).takeWhile(_ < pow(10, length))
  }

  def _rightTruncatablePrimesByLength(length: Int): Iterable[Int] =
    length match {
      case n if n < 2 => LazyList()
      case n if n == 2 =>
        primesByLength(2).filter(p => primesByLength(1).contains(p / 10))
      case n =>
        rightTruncatablePrimesByLength(n - 1).flatMap(a => primesByLength(n).filter(b => b.toString startsWith a.toString))
    }

  def rightTruncatablePrimesByLength: Int => Iterable[Int] = memoize(_rightTruncatablePrimesByLength)

  def _leftTruncatablePrimesByLength(length: Int): Iterable[Int] =
    length match {
      case n if n < 2 => LazyList()
      case n if n == 2 =>
        primesByLength(2).filter(p => primesByLength(1).contains(p % 10))
      case n =>
        leftTruncatablePrimesByLength(n - 1).flatMap(a => primesByLength(n).filter(b => b.toString endsWith a.toString))
    }

  def leftTruncatablePrimesByLength: Int => Iterable[Int] = memoize(_leftTruncatablePrimesByLength)

  val leftTruncatablePrimes = (2 to 6) flatMap leftTruncatablePrimesByLength
  val rightTruncatablePrimes = (2 to 6) flatMap rightTruncatablePrimesByLength
  val answer = leftTruncatablePrimes.intersect(rightTruncatablePrimes).take(11).sum
  println(answer)
}
