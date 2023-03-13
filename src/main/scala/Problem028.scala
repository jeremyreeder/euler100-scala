/* Problem 28: Number Spiral Diagonals
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13
It can be verified that the sum of the numbers on the diagonals is 101.
What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*/

import Math.pow

object Problem028 extends App {
	def cornerSum(length: Int) =
		length match
			case n if n < 1 => throw Exception("The diagonal length of a square spiral must be positive.")
			case 1 => 1
			case n if n % 2 == 0 => throw Exception("The diagonal length of a square spiral must be odd.")
			case n => 4 * pow(n, 2).toInt - 6 * n + 6
	
	val answer = (1 to 1001 by 2 map cornerSum).sum
	println(answer)
}
