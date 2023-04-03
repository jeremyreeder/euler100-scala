/* Problem 78: Coin Partitions
Let p(n) represent the number of different ways in which n coins can be separated into piles. For example, five coins
can be separated into piles in exactly seven different ways, so p(5)=7.

OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O
Find the least value of n for which p(n) is divisible by one million.
*/
import collection.mutable

object Problem078 extends App {
	def coinPartitionCounts(sum: Long) = {
		val memo = mutable.Map[(Long, Long), Long]()
		
		def count(sum: Long, summandMax: Long): Long = {
			if sum == 0 then 1L
			else if summandMax > sum then 0L
			else
				memo.get((sum, summandMax)) match {
					case Some(result) => result
					case None =>
						val result = count(sum - summandMax, summandMax) + count(sum, summandMax + 1)
						memo((sum, summandMax)) = result
						result
				}
		}
		
		count(sum, 1)
	}
	
	val answer = (LazyList from 1).find(coinPartitionCounts(_) % 1_000_000 == 0).get
	println(answer) // Never finished. TODO: Make it fast.
}
