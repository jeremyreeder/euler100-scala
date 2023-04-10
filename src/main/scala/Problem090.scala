object Problem090 extends App {
	
	val targets = // distinct pairs of digits which form a positive square, where 6 & 9 are NOT considered distinct
		(1 to 9).map(x => "%02d".format(x * x).replace('9', '6').sorted).distinct
	
	val cubePairs = // 6 & 9 are distinct but interchangeable
		def cubes = (0 to 9).combinations(6).map(_.mkString)
		(for a <- cubes; b <- cubes if b >= a yield (a, b)).distinct
	
	def hitsAllTargets(cubePair: (String, String)) = {
		def hitsTarget(cubePair: (String, String), target: String) =
			(cubePair._1.contains(target.head) && cubePair._2.contains(target.last))
				|| (cubePair._1.contains(target.last) && cubePair._2.contains(target.head))
		
		val cubesWith9sInverted = (cubePair._1.replace('9', '6'), cubePair._2.replace('9', '6'))
		!targets.exists(!hitsTarget(cubesWith9sInverted, _))
	}
	
	val answer = cubePairs.count(hitsAllTargets)
	println(answer)
}