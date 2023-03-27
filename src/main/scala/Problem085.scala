/* Problem 85: Counting Rectangles
By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:
  Six 1x1s, four 2x1s, two 2x3s, three 1x2s, two 2x2s, & one 3x2

Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with
the nearest solution.
*/
import util.control.Breaks.{break, breakable}

object Problem085 extends App {
	val sumOfOneTo =
		var sum = 0
		(LazyList from 0) map { n =>
			sum += n
			sum
		}
	
	def rectangleCount(gridWidth: Int, gridHeight: Int) =
		sumOfOneTo(gridWidth) * sumOfOneTo(gridHeight)
	
	def answer: Int =
		var Target = 2_000_000
		var minDiff = Int.MaxValue
		var closestGrid = (0, 0)
		for gridHeight <- LazyList from 1 do
			breakable {
				for gridWidth <- LazyList from 1 do
					val count = rectangleCount(gridWidth, gridHeight)
					val diff = (count - Target).abs
					if diff < minDiff then
						minDiff = diff
						closestGrid = (gridWidth, gridHeight)
					if count > Target then
						if gridWidth <= gridHeight then
							// We've seen all applicable grids. From here on, we'll just see the same grids, rotated.
							return closestGrid._1 * closestGrid._2
						break // Go to the next height.
			}
		throw Error("No answer found.") // This line is unreachable.
	
	println(answer)
}