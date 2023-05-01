import math.sqrt
import scala.annotation.tailrec

object Problem095 extends App {
	
	private val links = {
		var sumOfProperDivisors = {
			val max = 1_000_000
			for {
				a <- 2 to max
				b = (2 to sqrt(a).toInt).filter(a % _ == 0).flatMap(b1 => Seq(b1, a / b1)).distinct.map(_.toLong).sum + 1
					if b <= max
			} yield a -> b.toInt
		}.toMap
		
		for neverRepeated <- sumOfProperDivisors.keySet diff sumOfProperDivisors.values.toSet do
			sumOfProperDivisors -= neverRepeated
		
		sumOfProperDivisors
	}
	
	def chain(start: Int): Seq[Int] = {
		@tailrec
		def loop(acc: Vector[Int]): Vector[Int] =
			val next = links.getOrElse(acc.last, 0)
			if acc.tail contains next then Vector.empty
			else if next == acc.head then acc
			else loop(acc :+ next)
		
		loop(Vector(start))
	}
	
	val answer =
		val keyToLongestChain = links.keys.maxBy(chain(_).size)
		chain(keyToLongestChain).min
	
	println(answer)
}