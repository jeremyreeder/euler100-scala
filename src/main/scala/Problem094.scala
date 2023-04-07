/* Problem 94: Almost Equilateral Triangles
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However,
the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by
no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose
perimeters do not exceed one billion (1,000,000,000).
*/
object Problem094 extends App {

	val almostEquilateralIsoscelesTriples = {
		val abundantTriples =
			lazy val xs: LazyList[Long] = 5L #:: 65L #:: (LazyList from 2).map(i => 14 * xs(i - 1) - xs(i - 2) - 4)
			for x <- xs; y = x; z = x + 1
				yield (x, y, z)
		
		val deficientTriples =
			lazy val xs: LazyList[Long] = 17L #:: 241L #:: (LazyList from 2).map(i => 14 * xs(i - 1) - xs(i - 2) + 4)
			for x <- xs; y = x; z = x - 1
				yield (x, y, z)
		
		(abundantTriples zip deficientTriples).flatMap((at, dt) => List(at, dt))
	}
	
	val answer = almostEquilateralIsoscelesTriples.map(_ + _ + _).takeWhile(_ <= 1_000_000_000).sum
	println(answer)
}