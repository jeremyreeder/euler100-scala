/* Problem 43: Sub-string Divisibility
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order,
but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
	d[2] d[3] d[4] = 406 is divisible by 2
	d[3] d[4] d[5] = 063 is divisible by 3
	d[4] d[5] d[6] = 635 is divisible by 5
	d[5] d[6] d[7] = 357 is divisible by 7
	d[6] d[7] d[8] = 572 is divisible by 11
	d[7] d[8] d[9] = 728 is divisible by 13
	d[8] d[9] d[10] = 289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.
*/
object Problem043 extends App {
	def hasTheProperty(pandigital: String) =
		pandigital.slice(1, 4).toInt % 2 == 0
			&& pandigital.slice(2, 5).toInt % 3 == 0
			&& pandigital.slice(3, 6).toInt % 5 == 0
			&& pandigital.slice(4, 7).toInt % 7 == 0
			&& pandigital.slice(5, 8).toInt % 11 == 0
			&& pandigital.slice(6, 9).toInt % 13 == 0
			&& pandigital.slice(7, 10).toInt % 17 == 0
	
	val answer = "0123456789".permutations.filter(hasTheProperty).map(_.toLong).sum
	println(answer)
}
