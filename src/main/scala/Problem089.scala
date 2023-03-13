import scala.io.Source

object Problem089 extends App {
	val nameFile = Source fromFile "/home/jeremy/euler/22/names.txt"
	val namesText = nameFile.getLines.mkString
	nameFile.close()
	val namePattern = "(?<=\")([A-Z]+)(?=\")".r
	val names = namePattern.findAllIn(namesText).toList.sorted
	
	def score(name: String) =
		name.map(_ - 'A' + 1).sum * (names.indexOf(name) + 1)
	
	println(names.map(score).sum)
}
