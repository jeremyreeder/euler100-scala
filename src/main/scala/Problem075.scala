/* Problem 75: Singular Integer Right Triangles
It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in
exactly one way, but there are many more examples.
	12 cm: (3,4,5)
	24 cm: (6,8,10)
	30 cm: (5,12,13)
	36 cm: (9,12,15)
	40 cm: (8,15,17)
	48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other
lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three
different integer sided right angle triangles.
	120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one integer sided right angle
triangle be formed?
*/
import math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable

object Problem075 extends App {
	val Limit = 1_500_000
	
	@tailrec
	def gcd(a: Int, b: Int): Int =
		if b == 0 then a else gcd(b, a % b)
	
	lazy val tripleCounts = Array.fill(Limit + 1)(0)
	
	lazy val answer = // Euclid's formula
		for {
			m <- 2 to sqrt(Limit / 2).toInt
			n <- m-1 to 1 by -2
				if (gcd(m, n) == 1)
			ks = LazyList from 1
			perimeter <- ks.map(2 * _ * m * (m + n)).takeWhile(_ <= Limit)
		} do
			tripleCounts(perimeter) += 1
		tripleCounts.count(_ == 1)
	
	println(answer)
}