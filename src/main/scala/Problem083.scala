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
import scala.io.Source
import util.control.Breaks.{break, breakable}

object Problem083 extends App {
	val file = Source.fromFile("p082_matrix.txt")
	val matrix = file.getLines.map(_.split(",").map(_.toInt).toList).toList
	file.close()
	
	// Bell-Fordman algorithm, with early exit
	def minimalPathSum(matrix: List[List[Int]]): Int = {
		val (rows, columns) = (matrix.indices, matrix.head.indices)
		val distance = Array.tabulate(rows.size, columns.size) { (row, column) =>
			if row == 0 && column == 0 then
				matrix(row)(column)
			else Int.MaxValue
		}
		breakable {
			while true do {
				var isPathFreshlyShortened = false
				for {
					rowA <- rows
					columnA <- columns
					if distance(rowA)(columnA) != Int.MaxValue
					(rowB, columnB) <- List(
						(rowA - 1, columnA), // up
						(rowA + 1, columnA), // down
						(rowA, columnA - 1), // left
						(rowA, columnA + 1)  // right
					)
					if (rows contains rowB) && (columns contains columnB)
				} do
					val possiblyShorterDistance = distance(rowA)(columnA) + matrix(rowB)(columnB)
					if possiblyShorterDistance < distance(rowB)(columnB) then
						distance(rowB)(columnB) = possiblyShorterDistance
						isPathFreshlyShortened = true
				if !isPathFreshlyShortened then break
			}
		}
		distance(rows.last)(columns.last)
	}
	
	val answer = minimalPathSum(matrix)
	println(answer)
}