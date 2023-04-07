/* Problem 88: Product-sum Numbers
A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers,
{a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.
For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number. The minimal
product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.
	k=2: 4 = 2 × 2 = 2 + 2
	k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
	k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
	k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
	k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only counted once in
the sum.

In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.
What is the sum of all the minimal product-sum numbers for 2≤k≤12000?
*/
object Problem088 extends App {
	
	val Limit = 12_000
	val minSumProduct = Array.fill(Limit + 1)(0)
	
	def factorize(n: Int, remainder: Int, maxFactor: Int, sum: Int, terms: Int): Unit = {
		var k = terms
		if remainder == 1 then
			k += n - sum
			if k <= Limit && (minSumProduct(k) == 0 || n < minSumProduct(k)) then
				minSumProduct(k) = n
		else
			for factor <- 2 to maxFactor do
				if remainder % factor == 0 then
					factorize(n, remainder / factor, factor min maxFactor, sum + factor, k + 1)
	}
	
	for i <- 2 to 2 * Limit do
		factorize(i, i, i, 0, 0)
	
	val answer = minSumProduct.drop(2).distinct.sum
	println(answer)
}