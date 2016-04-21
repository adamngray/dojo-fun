object Chapter2 {

  //===============================
  // Ex2.1 Fibonacci Numbers
  def fib(n: Int): BigInt = {

    def go(n: Int, first: BigInt, second: BigInt): BigInt = {
      if (n <= 1) first
      else go(n - 1, second, first + second)
    }
    go(n,0,1)
  }

  fib(7)

  //===============================
  // Ex2.2

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n+1 >= as.length) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)

  }

  def isGreaterThan(a: Int, b:Int): Boolean = {
    if (a > b) true
    else false
  }

  val shortArray = Array(1,2)
  val mySortedArray = Array(1,2,3,4,5,6,7,8)
  val myUnsortedArray = Array(1,2,3,4,6,5,7)

  isSorted(shortArray, isGreaterThan)
  isSorted(mySortedArray, isGreaterThan)
  isSorted(myUnsortedArray, isGreaterThan)

  //===================================
  // Ex 2.3

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a:A => b:B => f(a,b)
  }

  //===================================
  // Ex 2.4

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  //=====================================
  // Ex 2.5

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a:A => f(g(a:A))
  }


}