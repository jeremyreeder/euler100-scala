/* Problem 100: Arranged Probability
If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken
at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box containing
eighty-five blue discs and thirty-five red discs.

By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, determine the number of blue
discs that the box would contain.
*/
import math.sqrt

object Problem100 extends App {
	
	lazy val bs: LazyList[Long] =
		15L #:: 85L #:: (LazyList from 2).map(n => 6 * bs(n - 1) - bs(n - 2) - 2)
	
	val dFloor = 1_000_000_000_000L
	val bFloor = (dFloor.toDouble / sqrt(2)).toLong
	val answer = bs.find(_ > bFloor).get
	println(answer)
	println((answer*sqrt(2)).toLong)
}