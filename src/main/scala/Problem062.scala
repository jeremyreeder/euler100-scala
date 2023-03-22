/* Problem 62: Cubic Permutations
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact,
41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/
import Math.cbrt
import System.currentTimeMillis

object Problem062 extends App {
	val cubes = LazyList.range(1L, cbrt(Long.MaxValue).toLong).map(n => n * n * n)
	
	def isCube(n: Long) =
		val root = cbrt(n)
		root == root.floor
	
	def answer: Option[Long] =
		for cube <- cubes do
			val largerCubicPermutations =
				cube.toString.permutations.map(_.toLong).filter(_ > cube).filter(isCube)
			if largerCubicPermutations.size == 4 then
				return Some(cube)
		None
	
	val startTime = currentTimeMillis()
	answer match {
		case Some(number) => println(number)
		case None => throw Error("No answer found.")
	}
	println(s"That took ${currentTimeMillis() - startTime} milliseconds.")
	
	// TODO: Speed this up. It's way too dang slow. I have not yet seen its answer.
}
