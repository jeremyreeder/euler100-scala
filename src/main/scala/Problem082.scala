/* Problem 82: Path Sum, Three Ways
The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and finishing in any cell in
the right column, and only moving up, down, and right, is indicated; the sum is equal to 994.
	 131   673  [234] [103] [ 18]
	[201] [ 96] [342]  965   150
	 630   803   746   422   111
	 537   699   497   121   956
	 805   732   524    37   331

Find the minimal path sum from the left column to the right column in matrix.txt, a 31K text file containing an 80 by 80
matrix.
*/
import io.Source

object Problem082 extends App {
	// This solution uses the original immutable matrix and a mutable temporary copy. It goes through the columns from
	// left to right, modifying the temporary matrix as it goes, such that the value in each cell is the minimal path sum
	// up to that point. In the end, the answer is therefore the smallest value found in the rightmost column of the
	// temporary matrix.
	
	val file = Source.fromFile("p082_matrix.txt")
	val matrix = file.getLines.map(_.split(",").map(_.toInt).toList).toList
	file.close()
	
	def clone(matrix: List[List[Int]]) =
		val result = Array.ofDim[Int](matrix.length, matrix.head.length)
		for i <- matrix.indices; j <- matrix.head.indices
			do result(i)(j) = matrix(i)(j)
		result
	
	val answer =
		val tempMatrix = clone(matrix)
		val (rows, columns) = (matrix.indices, matrix.head.indices)
		for { column <- columns.tail; row <- rows } do
			var minimalPathSum = tempMatrix(row)(column - 1) + matrix(row)(column)
			for startRow <- rows if startRow != row do
				val pathSegment = (startRow min row) to (startRow max row)
				val pathSum = tempMatrix(startRow)(column - 1) + pathSegment.map(matrix(_)(column)).sum
				minimalPathSum = minimalPathSum min pathSum
			tempMatrix(row)(column) = minimalPathSum
		rows.map(tempMatrix(_)(columns.last)).min
	
	println(answer)
}