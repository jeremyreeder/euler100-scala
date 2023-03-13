/* Problem 4: Largest palindrome product
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
9009 = 91 × 99. Find the largest palindrome made from the product of two 3-digit numbers.
*/
object Problem004 extends App {
	private val palindromicProducts =
		for a <- 100 to 999
		    b <- 100 to 999
		    c = a * b
		    s = c.toString
		    if s.reverse == s
		yield c
	println(palindromicProducts.max)
}
