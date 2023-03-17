object Problem057 extends App {
	val limit = 1000
	val one = BigInt(1)
	val two = BigInt(2)
	var denominator = one
	var x = two
	var count = 0
	
	for (i <- 1 to limit) {
		val nextDenominator = x
		val nextX = x * two + denominator
		denominator = nextDenominator
		x = nextX
		val numerator = x - denominator
		if (numerator.toString.length > denominator.toString.length) count += 1
	}
	
	println(count)
}
