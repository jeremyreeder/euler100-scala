/* Problem 5: Smallest Multiple
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/
object Problem005 extends App {
  def isDivisibleByAllMyDivisors(n: Int) =
    val myDivisors = 1 to 20
    if myDivisors exists (n % _ != 0) then false else true
  val divisibleNumbers = {LazyList from 1} dropWhile {!isDivisibleByAllMyDivisors(_)}
  println(divisibleNumbers(0))
}
