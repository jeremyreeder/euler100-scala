/* Problem 39: Integer Right Triangles
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions
for p = 120.
  {20,48,52}, {24,45,51}, {30,40,50}
For which value of p ≤ 1000, is the number of solutions maximised?
*/
import Math.pow
object Problem039 extends App {
  def solutions(p: Int) =
    for a <- 1 until p / 2
        c <- a + 1 until p - 1
        b = p - a - c
        if pow(a, 2) + pow(b, 2) == pow(c, 2)
      yield (a, b, c)

  val answer = (12 to 1_000).maxBy(solutions(_).size)
  println(answer)
}
