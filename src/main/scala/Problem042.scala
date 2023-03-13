/* Problem 42: Coded Triangle Numbers
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we
form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle
number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common
English words, how many are triangle words?
*/
import io.Source
object Problem042 extends App {
  val words =
    // Parse a file consisting of a series of quoted words, separated by commas.
    Source.fromFile("words.txt").mkString.split(",").map(_.drop(1).dropRight(1)).toList

  def value(word: String) =
    // Words are in all caps. '@' immediately precedes 'A' in ASCII. Values depend on how far after '@' the letters are.
    word.map(_ - '@').sum

  val maxWordValue = words.map(value).max
  val triangularNumbers = (LazyList from 1).map(n => n * (n + 1) / 2).takeWhile(_ <= maxWordValue)
  val triangularWords = words.filter(triangularNumbers contains value(_))
  val answer = triangularWords.size
  println(answer)
}
