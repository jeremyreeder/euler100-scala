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
object Problem078 extends App {
	
	def partition(n: Int): BigInt = {
		val cache = Array.fill[BigInt](n + 1)(-1)
		
		def loop(m: Int): BigInt = {
			if m < 0 then 0
			else if m == 0 then 1
			else if cache(m) != -1 then cache(m)
			else
				var sum = BigInt(0)
				var k = 1
				while (true) do
					val pentagonal1 = k * (3 * k - 1) / 2
					val pentagonal2 = k * (3 * k + 1) / 2
					if pentagonal1 > m then return sum
					sum += (if k % 2 == 0 then -1 else 1) * (loop(m - pentagonal1) + loop(m - pentagonal2))
					k += 1
				cache(m) = sum
				sum
		}
		
		loop(n)
	}
	
	val answer = (LazyList from 1).find(partition(_) % 1_000_000 == 0)
	println(answer) // Still way to slow. TODO: Make it fast.
}