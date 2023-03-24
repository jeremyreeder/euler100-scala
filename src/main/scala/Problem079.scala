/* Problem 79: Passcode Derivation
A common security method used for online banking is to ask the user for three random characters from a passcode. For
example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible
secret passcode of unknown length.
*/
import io.Source

object Problem079 extends App {
	
	val file = Source.fromFile("079_keylog.txt")
	val oneTimePasscodes = file.getLines().toSet
	file.close()
	
	def answer: String =
		var chars = oneTimePasscodes.mkString.distinct
		var dependencies = oneTimePasscodes.flatMap(otp => Seq(otp(1) -> otp(0), otp(2) -> otp(1), otp(2) -> otp(0)))
		var fullPasscode = ""
		while !chars.isEmpty do
			val unblockedChar = chars.find(c => !dependencies.exists(_._1 == c)).get
			fullPasscode += unblockedChar
			chars = chars.replaceFirst(unblockedChar.toString, "")
			dependencies = dependencies.filter(_._2 != unblockedChar)
		fullPasscode
	
	println(answer)
}