/* Problem 76: Counting Summations
It is possible to write five as a sum in exactly six different ways:
	4 + 1
	3 + 2
	3 + 1 + 1
	2 + 2 + 1
	2 + 1 + 1 + 1
	1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?
*/
import scala.annotation.tailrec
import scala.collection.mutable

object Problem076 extends App {
	def countSummations(sum: Int) = {
		val memo = mutable.Map[(Int, Int), Int]()
		
		def count(sum: Int, summandMax: Int): Int = {
			if sum == 0 then 1
			else if summandMax > sum then 0
			else memo.getOrElseUpdate((sum, summandMax), count(sum - summandMax, summandMax) + count(sum, summandMax + 1))
		}
		
		count(sum, 1) - 1
	}
	
	println(countSummations(100))
}