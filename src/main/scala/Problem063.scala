/* Problem 63: Powerful Digit Counts
The 5-digit number, 16807 = 7^5, is also a fifth power. Similarly, the 9-digit number, 134217728 = 8^9, is a ninth power.
How many n-digit positive integers exist which are also an nth power?
*/
import Math.log

object Problem063 extends App {
	// 10^(n-1) <= x^n < 10^n  ->  x[max]^n < 10^n  ->  x[max] < 10
	val xs = 1 to 9
	
	def ns(x: Int) = {
		val maxN = x match { // 10^(n-1) <= x^n < 10^n
			case 1 => 1 // n <= 1
			case 2 => 1 // n <= (log(2)+log(5))/log(5) ~= 1.4
			case 3 => 1 // n <= (log(2)+log(5))/(log(2)-log(3)+log(5)) ~= 1.9
			case 4 => 2 // n <= (-log(2)-log(5))/(log(2)-log(5)) ~= 2.5
			case 5 => 3 // n <= (log(2)+log(5))/log(2) ~= 3.3
			case 6 => 4 // n <= (-log(2)-log(5))/(log(3)-log(5)) ~= 4.5
			case 7 => 6 // n <= (log(2)+log(5))/(log(2)+log(5)-log(7)) ~= 6.5
			case 8 => 10 // n <= (-log(2)-log(5))/(2log(2)-log(5)) ~= 10.3
			case 9 => 21 // n <= (log(2)+log(5))/(log(2)-2log(3)+log(5)) ~= 21.9
			case _ => 0
		}
		
		def minN(x: Int) =
			val bigX = BigInt(x)
			(1 to maxN).find(n => (bigX pow n).toString.length == n).get
		
		minN(x) to maxN
	}
	
	val answer = // Count the values of x^n for which the length is exactly n digits.
		xs.map(ns(_).size).sum
	
	println(answer)
}
/* Fully mathematical solution:
The problem can be restated as 10^(n-1) <= x^n < 10^n.

x^n < 10^n can be simplified to x < 10. So the only possible values of x are 1 thru 9.

For each value of x, I solved for n:
	Given x=1, 1 <= n <= 1.
	Given x=2, 1 <= n <= (log(2)+log(5))/log(5) ~= 1.4.
	Given x=3, 1 <= n <= (log(2)+log(5))/(log(2)-log(3)+log(5)) ~= 1.9.
	Given x=4, 1 <= n <= (-log(2)-log(5))/(log(2)-log(5)) ~= 2.5.
	Given x=5, 1 <= n <= (log(2)+log(5))/log(2) ~= 3.3.
	Given x=6, 1 <= n <= (-log(2)-log(5))/(log(3)-log(5)) ~= 4.5.
	Given x=7, 1 <= n <= (log(2)+log(5))/(log(2)+log(5)-log(7)) ~= 6.5.
	Given x=8, 1 <= n <= (-log(2)-log(5))/(2log(2)-log(5)) ~= 10.3.
	Given x=9, 1 <= n <= (log(2)+log(5))/(log(2)-2log(3)+log(5)) ~= 21.9.

Since every range starts at 1 and contains only integers, the answer is obtained by simply rounding each max[n] down and summing them up.
	1+1+1+2+3+4+6+10+21 = 49
*/