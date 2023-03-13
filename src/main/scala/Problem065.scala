object Problem065 extends App {
	val cycle = LazyList.range(2, Int.MaxValue, 2).flatMap(List(1, _, 1))
	var (a, b) = (BigInt(1), BigInt(2))
	cycle take 99 foreach { i =>
		val c = a + b * i
		a = b
		b = c
	}
	println(b.toString.map(_ - '0').sum)
}
