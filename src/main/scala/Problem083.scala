/* Problem 83: Path Sum, Four Ways
In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by moving left, right, up, and
down, is indicated and is equal to 2297.
	[131]  673  [234] [103] [ 18]
	[201] [ 96] [342]  965  [150]
	 630   803   746  [422] [111]
	 573   699   497  [121]  956
	 805   732   524  [ 37] [331]
 
Find the minimal path sum from the top left to the bottom right by moving left, right, up, and down in matrix.txt, a 31K
text file containing an 80 by 80 matrix.
*/

object Problem083 extends App {
	val matrix = // TODO: replace example matrix with file contents
		List(
			List(131, 673, 234, 103,  18),
			List(201,  96, 342, 965, 150),
			List(630, 803, 746, 422, 111),
			List(573, 699, 497, 121, 956),
			List(805, 732, 524,  37, 331)
		)
	
	val (rows, columns) = (matrix.indices, matrix.head.indices)
	
	def minimumSumToEnd(row: Int, column: Int): Int =
		if row == rows.last && column == columns.last then
			return matrix(row)(column)
		if !(rows contains row) || !(columns contains column) then
			return Int.MaxValue
		List(
			minimumSumToEnd(row, column - 1), // left
			minimumSumToEnd(row, column + 1), // right
			minimumSumToEnd(row - 1, column), // up
			minimumSumToEnd(row + 1, column)  // down
		).min
		
	val answer = minimumSumToEnd(0, 0)
	println(answer)
}