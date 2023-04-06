/* Problem 94: Almost Equilateral Triangles
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However,
the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by
no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose
perimeters do not exceed one billion (1,000,000,000).
*/
import math.sqrt

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
			a <- 3L to MaxPerimeter / 6 + 1L
			c <- Seq(2L * a - 1L, 2L * a + 1L)
			b = sqrt(c * c - a * a)
				if b == b.floor
		} yield (a, b.toLong, c)
	
	def swapAxes(pythagoreanTriple: (Long, Long, Long)) =
		val (x, y, z) = pythagoreanTriple
		(y, x, z)
	
	def almostEquilateralIsosceleseHeronianTriples =
		for {
			(a, b, c) <- heronianPythagoreanTriples.flatMap(triple => Seq(triple, swapAxes(triple)))
			(x, y, z) = (c, c, 2L * a)
			if (z - x).abs == 1
		} yield
			(x, y, z)
	
	def perimeter(triangle: (Long, Long, Long)) =
		val (x, y, z) = triangle
		x + y + z
	
	val startTime = System.currentTimeMillis()
	
	println(heronianPythagoreanTriples.take(5))
	println(almostEquilateralIsosceleseHeronianTriples.take(5))
	
	val answer =
		almostEquilateralIsosceleseHeronianTriples
			.map(perimeter)
			.takeWhile(_ <= MaxPerimeter)
			.sum
	
	println(answer)
	/* 5_479_171_588 is incorrect. So is 2_664_374_306. Are there other almost-equilateral isosceles Heronian triangles
	besides the ones formed by combining two Heronian Pythagorean triangles? */
	
	println(System.currentTimeMillis() - startTime) // As of 2023-04-06T08, this takes 83 seconds.
}