/* Problem 66: Diophantine Equation
Consider quadratic Diophantine equations of the form:
	x^2 – D*y^2 = 1

For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
It can be assumed that there are no solutions in positive integers when D is square.
By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
	3^2 – 2×2^2 = 1
	2^2 – 3×1^2 = 1
	9^2 – 5×4^2 = 1
	5^2 – 6×2^2 = 1
	8^2 – 7×3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
*/
object Problem066 extends App {
	
	def findD_withGreatestMinimalX(dLimit: Int) = { // Lagrange method
		val (zero, one) = (BigInt(0), BigInt(1))
		var result = 0; var greatestMinimalX = zero
		for d <- 2 to dLimit do {
			val sqrtD = math.sqrt(d).toInt
			if sqrtD * sqrtD != d then
				var (i, j, k) = (zero, one, BigInt(sqrtD))
				var num1 = one
				var num = k
				var den1 = zero
				var den = one
				while num * num - d * den * den != 1 do
					i = j * k - i
					j = (d - i * i) / j
					k = (sqrtD + i) / j
					val num2 = num1;
					val den2 = den1
					num1 = num;
					den1 = den
					num = k * num1 + num2;
					den = k * den1 + den2;
				if num > greatestMinimalX then
					greatestMinimalX = num
					result = d
		}
		result
	}

	val answer = findD_withGreatestMinimalX(dLimit = 1000)
	println(answer)
}