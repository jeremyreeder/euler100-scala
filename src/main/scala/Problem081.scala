/* Path Sum: Two Ways
In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and
down, is indicated in bold red and is equal to 2427.

Find the minimal path sum from the top left to the bottom right by only moving right and down in matrix.txt, a 31K text
file containing an 80 by 80 matrix.
*/
import scala.io.Source

object Problem081 extends App {
	// This solution involves modifying the matrix in place until the value in every cell represents the minimum sum of
	// all paths from the beginning up to that point.
	
	val file = Source.fromFile("081_matrix.txt")
	val matrix = file.getLines.map(_.split(",").map(_.toInt)).toArray
	file.close()
	
	val answer =
		for {
			i <- matrix.indices
			j <- matrix(i).indices
			if (i, j) != (0, 0)
		} do
			matrix(i)(j) +=
				math.min(
					if i > 0 then matrix(i - 1)(j) else Int.MaxValue,
					if j > 0 then matrix(i)(j - 1) else Int.MaxValue
				)
		matrix.last.last
	
	println(answer)
}