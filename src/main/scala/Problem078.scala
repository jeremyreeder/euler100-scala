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
import util.control.Breaks.{break, breakable}

object Problem078 extends App {
	
	def answer: Option[Int] =
		val partitions = mutable.ArrayBuffer[BigInt]()
		partitions += 1
		for n <- LazyList from 1 do
			var (k, sum) = (1, BigInt(0))
			breakable {
				while (true) do
					val pent1 = k * (3 * k - 1) / 2
					val pent2 = k * (3 * k + 1) / 2
					if pent1 > n then break
					val (i, j) = (n - pent1, n - pent2)
					val partI = if i < 0 then BigInt(0) else partitions(i)
					val partJ = if j < 0 then BigInt(0) else partitions(j)
					sum += (if k % 2 == 0 then -1 else 1) * (partI + partJ)
					k += 1
			}
			if sum % 1_000_000 == 0 then return Some(n)
			else partitions += sum
		None
	
	println(answer.get)
}