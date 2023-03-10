import scala.annotation.tailrec

@tailrec
def fibonacci(num: Int, n1: BigInt, n2: BigInt): BigInt = if (num == 0) 0 else if (num == 1) n2 else fibonacci(num - 1, n2, n1 + n2)
val fibBigInt: Int => BigInt = num => fibonacci(num, 0, 1)

println(fibBigInt(1))
println(fibBigInt(0))
println(fibBigInt(100000))