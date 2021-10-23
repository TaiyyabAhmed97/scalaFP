object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def fibonacci(n: Int): Int = {
    def go(n: Int): Int = {
      if (n == 0) 0
      else if (n == 2 || n == 1) 1
      else go(n - 1) + go(n - 2)
    }
    go(n)
  }

  def factorial(x: Int): Int = {
    def go(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else go(x - 1, acc * x)
    }
    go(x, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d"
    msg.format(name, n, f(n))
  }

  private def formatAbs(x: Int) = {
    val msg = "The abs of %d is %d"
    msg.format(x, abs(x))

  }

  private def formatFib(x: Int) = {
    val msg = "The %dth fibonacci number is %d"
    msg.format(x, fibonacci(x - 1))
  }

  private def formatFac(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n - 1 >= as.length) true
      else if (!ordered(as(n - 2), as(n - 1))) false
      else {
        println(n)
        loop(n + 1)
      }
    }
    loop(2)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => (b: B) =>
    f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): (A, B) => C = { (a: A, b: B) =>
    f(g(a))
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x + product(xs)

    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def setHead[A](list: List[A], first: A): List[A] = list match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(first, tail)
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Nil         => Nil
      case Cons(x, xs) => xs
    }

    def drop[A](list: List[A], n: Int): List[A] = {
      def loop(n: Int, list: List[A]): List[A] = {
        if (n <= 0) list
        else loop(n - 1, tail(list))
      }
      loop(n, list)
    }
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))

    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

    def foldLeftByRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      foldRight(as, (b: B) => b)((a, g) => b => (g(f(b, a))))(z)
    }

    def length[A](ns: List[A]): Int = {
      foldRight(ns, 0)((x, y) => y + 1)
    }

    def sum3(ns: List[Int]): Int = {
      foldLeft(ns, 0)((y, x) => x + y)
    }

    def product3(ns: List[Int]): Int = {
      foldLeft(ns, 1)((x, y) => y * x)
    }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Int]) =
      foldRight(ns, 1)((x, y) => x * y)

    def reverse[A](ns: List[A]): List[A] = {
      foldLeft(ns, List[A]())((acc, h) => Cons(h, acc))
    }
  }

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None      => None
      case Some(get) => Some(f(get))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case None      => default
      case Some(get) => get
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      if (this == None) ob
      this
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _               => None
    }
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }
    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (cc => b map (dd => f(cc, dd)))
  }

  def main(args: Array[String]): Unit = {
    println("-" * 50)
    val abeer = List(4, 5, 6)
    val shoppingCart = 0
    println(abeer)
    println(Cons(List(1, 2, 3), List(4)))
    val ints = Array(1, 2, 3, 4, 5, 3, 7, 8, 9, 10)
    val compare = (x: Int, y: Int) => if (x < y) true else false
    val curried = curry(compare)
    val oemore = curried(32)
    val res = Some(45).map((x) => x * 2)
    val op1 = Some(10)
    val op2 = Some(20)
    println(map2(op1, op2)((a, b) => a * b))
    println(Some(12).flatMap((a => Some(a * 7))))
    val abs0: Option[Double] => Option[Double] = lift(math.abs)
    println(abs0(Some(-12)))
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 101
    }
    val list = List(1, 2, 3, 4, 5)
    println(List.reverse(list))
    println("-" * 50)
  }
}
