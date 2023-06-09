/* Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and
are generated by the following formulae:
	Triangle    P[3,n]=n(n+1)/2   1, 3, 6, 10, 15, ...
	Square      P[4,n]=n^2        1, 4, 9, 16, 25, ...
	Pentagonal 	P[5,n]=n(3n−1)/2  1, 5, 12, 22, 35, ...
	Hexagonal   P[6,n]=n(2n−1)    1, 6, 15, 28, 45, ...
	Heptagonal  P[7,n]=n(5n−3)/2  1, 7, 18, 34, 55, ...
	Octagonal   P[8,n]=n(3n−2)    1, 8, 21, 40, 65, ...

The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
	1. The set is cyclic, in that the last two digits of each number is the first two digits of the next number
		(including the last number with the first).
	2. Each polygonal type: triangle (P[3,127]=8128), square (P[4,91]=8281), and pentagonal (P[5,44]=2882), is
		represented by a different number in the set.
	3. This is the only set of 4-digit numbers with this property.

Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square,
pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.
*/
object Problem061 extends App {
	
	val buckets =
		val inputRange = LazyList.range(0, Int.MaxValue)
		def constrict(range: Iterable[Int]) = range.dropWhile(_ < 1010).filter(_ % 100 >= 10).takeWhile (_ <= 9999)
		List(
			constrict(inputRange.map(n => n * (n + 1) / 2)),     // triangles
			constrict(inputRange.map(n => n * n)),               // squares
			constrict(inputRange.map(n => n * (3 * n - 1) / 2)), // pentagons
			constrict(inputRange.map(n => n * (2 * n - 1))),     // hexagons
			constrict(inputRange.map(n => n * (5 * n - 3) / 2)), // heptagons
			constrict(inputRange.map(n => n * (3 * n - 2)))      // octogons
		)
	
	def isCyclic(sequence: Seq[Int]) =
		for {
			i <- 0 until sequence.size
			j = (i + 1) % sequence.size
		} do sequence(i) % 100 == sequence(j) / 100
	
	def answer: Option[Int] =
		for bucketOrder <- (0 to 5).permutations do
			for {
				a <- buckets(bucketOrder(0))
				fa = a / 100
				ab = a % 100
			} do for {
				b <- buckets(bucketOrder(1)).dropWhile(_ <= ab * 100).takeWhile(_ < (ab + 1) * 100)
					.filter(_ != a)
				bc = b % 100
			} do for {
				c <- buckets(bucketOrder(2)).dropWhile(_ <= bc * 100).takeWhile(_ < (bc + 1) * 100)
					.filterNot(Seq(a, b) contains _)
				cd = c % 100
			} do for {
				d <- buckets(bucketOrder(3)).dropWhile(_ <= cd * 100).takeWhile(_ < (cd + 1) * 100)
					.filterNot(Seq(a, b, c) contains _)
				de = d % 100
			} do for {
				e <- buckets(bucketOrder(4)).dropWhile(_ <= de * 100).takeWhile(_ < (de + 1) * 100)
					.filterNot(Seq(a, b, c, d) contains _)
				ef = e % 100
			} do for {
				f <- buckets(bucketOrder(5)).dropWhile(_ <= ef * 100).takeWhile(_ < (ef + 1) * 100)
					.filterNot(Seq(a, b, c, d, e) contains _)
					.filter(_ % 100 == fa)
			} do return Some(a + b + c + d + e + f)
		None
	
	answer match {
		case Some(number) => println(number)
		case None => throw Error("Found no answer.")
	}
}