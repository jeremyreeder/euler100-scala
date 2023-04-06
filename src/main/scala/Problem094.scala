/* Problem 94: Almost Equilateral Triangles
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However,
the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by
no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose
perimeters do not exceed one billion (1,000,000,000).
*/
import math.{pow, sqrt}

object Problem094 extends App {

	/* The triangles described are the almost-equilateral isoscelese Heronian triangles, each of which is formed by
	combining a Pythagorean triangle with its mirror image. For this to work out, the Pythagorean triangle must be one
	whose hypotenuse C differs from twice its shortest leg A by exactly 1. I'll call these Heronian Pythagorean
	triangles. The first Pythagorean triangle, (3, 4, 5), is such a triangle. Since the Pythagorean triangle gets
	combined with the mirror image of itself, the resulting area is equivalent to the area of a rectangular with
	integral sides, making the area integral as well. */
	
	val MaxPerimeter = 1_000_000_000L // TODO: needs to handle a billion
	
	def heronianPythagoreanTriples =
		for {
			a <- 3L to MaxPerimeter / 6 + 1
			c <- Seq(a * 2 - 1, a * 2 + 1)
			b = sqrt(pow(c, 2) - pow(a, 2)) // too little precision in a Double, perhaps?
				if b == b.floor
		} yield (a, c) // b is unused hereafter
	
	def almostEquilateralIsoscelesHeronianTriples =
		for {
			(a, c) <- heronianPythagoreanTriples
			(x, z) = (c, a * 2)
		} yield
			(x, z) // y is redundant, since it's equal to x
	
	def perimeter(triangle: (Long, Long)) =
		val (x, z) = triangle
		BigInt(x) * 2 + z
	
	val startTime = System.currentTimeMillis()
	
	println(heronianPythagoreanTriples.takeRight(5))
	println(almostEquilateralIsoscelesHeronianTriples.take(5))
	
	val answer = almostEquilateralIsoscelesHeronianTriples.map(perimeter).takeWhile(_ <= MaxPerimeter).sum
	
	println(answer)
	// All I've got so far are incorrect answers:
	// 2_664_374_306, 4_489_548_014, 5_479_171_588, 11_138_120_726, 3_636_997_891_390
	
	println(System.currentTimeMillis() - startTime) // As of 2023-04-06T08, this takes 83 seconds.
}