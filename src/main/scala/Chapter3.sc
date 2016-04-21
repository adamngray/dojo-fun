object Chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Ex 3.2
    def tail[A](ts: List[A]): List[A] = ts match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    // Ex 3.3
    def setHead[A](ss: List[A], s: A): List[A] = ss match {
      case Nil => Cons(s, ss)
      case Cons(x, xs) => Cons(s, xs)
    }

    // Ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (n == 1) xs
        else drop(xs, n-1)
      }
    }

    // Ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) dropWhile(xs, f)
        else xs
      }
    }

    // Ex 3.6 - return everything but last value
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  }

  //=============================
  // Ex 3.1

  // Answer: 3

  val exlist = List(1,2,3,4,5,6)

  // Ex 3.2
  val ex2answ = List.tail(exlist)

  // Ex 3.3
  val ex3answ = List.setHead(exlist, 7)

  // Ex 3.4
  val ex4answ = List.drop(exlist, 4)

  // Ex 3.5
  def lessthan3(a: Int): Boolean = if (a<3) true else false
  val ex5answ = List.dropWhile(exlist, lessthan3)

  // Ex 3.6
  val ex6answ = List.init(exlist)






}