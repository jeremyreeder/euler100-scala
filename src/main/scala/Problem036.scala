/* Problem 36: Double-base Palindromes
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
(Please note that the palindromic number, in either base, may not include leading zeros.)
*/
object Problem036 extends App {
  def isDoublePalindrome(n: Int) =
    { val d = n.toString; d.reverse == d } && { val b = n.toBinaryString; b.reverse == b }
  val answer = (1 until 1_000_000).filter(isDoublePalindrome).sum
  println(answer)
}
