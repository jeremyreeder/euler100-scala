/* Problem 62: Cubic Permutations
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact,
41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/
import Math.cbrt
import util.control.Breaks.{break, breakable}

object Problem062 extends App {
	val cubes = LazyList.range(1, Int.MaxValue).map(n => BigInt(n) * n * n)
	
	def isPerfectCube(n: BigInt) =
		val root = cbrt(n.toDouble)
		root == root.floor
	
	var answer = BigInt(0)
	breakable {
		for cube <- cubes do
			val permutations = cube.toString.permutations.filterNot(_.head == '0').map(BigInt.apply)
			val cubicPermutations = permutations.filter(isPerfectCube).toList
			if cubicPermutations.size == 5 then
				println(cubicPermutations)
				answer = cubicPermutations.min
				break
	}
	println(answer)
	
	// TODO: Speed this up. It's way too dang slow. I have not yet seen its answer.
}
