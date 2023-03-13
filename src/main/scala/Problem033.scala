object Problem033 extends App {
	def fractions =
		for a1 <- 1 to 9
		    b1 <- a1 + 1 to 9
		    c <- 1 to 9
		    a2 = 10 * a1 + c
		    b2 = 10 * c + b1
		    if a1.toDouble / b1 == a2.toDouble / b2
		yield (a1, b1)
	
	val (as, bs) = fractions.unzip
	val product = (as.product, bs.product)
	val answer = product._2 / product._1
	println(answer)
}