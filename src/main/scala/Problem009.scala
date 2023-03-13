/* Problem 9: Special Pythagorean Triplet
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  a^2 + b^2 = c^2
  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/

import Math.pow

object Problem009 extends App {
	private def pythagoreanTriplets =
		for a <- 1 to 1000
		    b <- a + 1 to 1000 - a
		    c <- b + 1 to 1000 - a - b
		    if pow(a, 2) + pow(b, 2) == pow(c, 2)
		yield (a, b, c)
	
	private def special(a: Int, b: Int, c: Int) =
		a + b + c == 1000
	
	val answer = pythagoreanTriplets.find(special).get.toList.product
	println(answer)
}
