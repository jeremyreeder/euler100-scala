/* Problem 1: Multiples of 3 or 5
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6, and 9.
The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.
*/
object Problem001 extends App {
	def answer(limit: Int) = ((3 until limit by 3) ++ (5 until limit by 5)).distinct.sum
	
	for limit <- Seq(10, 100) do
		println(s"The sum of all multiples of 3 or 5 below $limit is ${answer(limit)}.")
}
