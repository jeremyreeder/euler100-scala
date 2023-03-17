/* Problem 59: XOR Decryption
Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for
Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value,
taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random
bytes. The user would keep the encrypted message and the encryption key in different locations, and without both
"halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the
password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The
balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case characters. Using 059_cipher.txt, a
file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words,
decrypt the message and find the sum of the ASCII values in the original text.
*/
import scala.io.Source

object Problem059 extends App {
	
	val ciphertext = Source.fromFile("059_cipher.txt").mkString.split(',').map(_.toInt.toChar).mkString
	
	def decrypt(ciphertext: String, key: String) =
		(0 until ciphertext.length).map(pos => (ciphertext(pos) ^ key(pos % key.length)).toChar).mkString
	
	val possibleKeys = for {k1 <- 'a' to 'z'; k2 <- 'a' to 'z'; k3 <- 'a' to 'z'} yield s"$k1$k2$k3"
	var plaintext = ""
	var maxLowercaseLetters = 0
	for key <- possibleKeys do
		val possiblePlaintext = decrypt(ciphertext, key)
		val lowerCaseLetterCount = possiblePlaintext.count('a' to 'z' contains _)
		if lowerCaseLetterCount > maxLowercaseLetters then
			plaintext = possiblePlaintext
			maxLowercaseLetters = lowerCaseLetterCount
	val answer = plaintext.map(_.toInt).sum
	println(answer)
	
	// TODO: Make it faster. The goal is 5 seconds.
}