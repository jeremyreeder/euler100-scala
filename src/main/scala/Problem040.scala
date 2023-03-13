/* Problem 40: Champernowne's Constant
An irrational decimal fraction is created by concatenating the positive integers:
  0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If d[n] represents the nth digit of the fractional part, find the value of the following expression.
  d[1] × d[10] × d[100] × d[1000] × d[10000] × d[100000] × d[1000000]
*/
object Problem040 extends App {
  val d = (0 to 200_000).mkString.take(1_000_001).map(_ - '0')
  val answer = d(1) * d(10) * d(100) * d(1_000) * d(10_000) * d(100_000) * d(1_000_000)
  println(answer)
}
