/* Problem 91: Right Triangles with Integer Coordinates
The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O (0,0), to form
ΔOPQ.

There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0
and 2 inclusive; that is,
	0 ≤ x1, y1, x2, y2 ≤ 2.

Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
*/
import math.{pow, sqrt}

object Problem091 extends App {
	val Limit = 50
	
	def distance(p: (Int, Int), q: (Int, Int)): Double =
		(p, q) match {
			case ((x1, y1), (x2, y2)) if x1 == x2 =>
				(y1 - y2).abs
			case ((x1, y1), (x2, y2)) if y1 == y2 =>
				(x1 - x2).abs
			case ((x1, y1), (x2, y2)) =>
				sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
		}
	
	def isRightTriangle(o: (Int, Int), p: (Int, Int), q: (Int, Int)) =
		val op = distance(o, p)
		val pq = distance(p, q)
		val qo = distance(q, o)
		Seq(op, pq, qo).sorted match {
			case Seq(a, b, c) =>
				(pow(a, 2) + pow(b, 2) - pow(c, 2)).abs < 0.001 // Pythagorean Theorem
			case _ => false
		}
	
	val answer = {
		val o = (0, 0)
		for {
			x1 <- 0 to Limit; y1 <- 0 to Limit; p = (x1, y1)
				if p != o // no duplicate points allowed
			x2 <- x1 to Limit; y2 <- 0 to Limit; q = (x2, y2)
				if q != o && q != p // no duplicate points allowed
				if isRightTriangle(o, p, q)
		} yield Seq(p, q).sorted
	}.distinct.size
	
	println(answer)
}