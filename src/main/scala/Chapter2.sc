object Chapter2 {

  // Ex2.1 Fibonacci Numbers
  def fib(n: Int): BigInt = {

    def go(n: Int, first: BigInt, second: BigInt): BigInt = {
      if (n <= 0) second
      else go(n - 1, first + second, second)
    }
    go(n,0,1)
  }

  val thingy = fib(12)
  val thing2 = fib(1)

  // Factorials
  def fact(n: Int): BigInt = {
    def go(n:Int, acc:BigInt): BigInt = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  val factorialval = fact(10)

  // Ex2.2


}