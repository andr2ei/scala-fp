package scala.com.fp.part1.chapter3


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

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(res: List[A], i: Int): List[A] = {
      if (i == n) return res
      go(tail(res), i + 1)
    }

    go(l, n)
  }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  // exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) if xs == Cons(x, Nil) => Cons(x, Nil)
      case Cons(x, xs) => append(List(x), init(xs))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a: A, b: Int) => 1 + b)

  // exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(bs: List[A], w: B, b: B): B = {
      if (length(as) == 0) return w
      bs match {
        case Nil => b
        case Cons(x, xs) => go(xs, w, f(b, x))
      }
    }

    go(as, z, z)
  }

  // exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  // exercise 3.11
  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  // exercise 3.11
  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((b: Int, a: A) => 1 + b)

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((as: List[A], b: A) => Cons(b, as))
  }

  // exercise 3.13
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((b, a) => f(a, b))
  }

  // exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a2), a1)((b, a) => Cons(a, b))
  }

  // exercise 3.15
  def flatMap[A](l: List[List[A]]): List[A] = {
    def go(l: List[List[A]]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => append(x, go(xs))
      }
    }

    go(l)
  }

  // exercise 3.16
  def incrementList(as: List[Int]): List[Int] = {
    def loop(bs: List[Int]): List[Int] = {
      bs match {
        case Nil => Nil
        case Cons(x: Int, xs: List[Int]) => Cons(x + 1, loop(xs))
      }
    }

    loop(as)
  }

  // exercise 3.17
  def doubleToStr(as: List[Double]): List[String] = {
    def loop(bs: List[Double]): List[String] = {
      bs match {
        case Nil => Nil
        case Cons(x: Double, xs: List[Double]) => Cons(x.toString, loop(xs))
      }
    }

    loop(as)
  }

  // exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    def go(bs: List[A]): List[B] = {
      bs match {
        case Nil => Nil
        case Cons(x: A, xs: List[A]) => Cons(f(x), go(xs))
      }
    }

    go(as)
  }

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def go(bs: List[A]): List[A] = {
      bs match {
        case Nil => Nil
        case Cons(x: A, xs: List[A]) => {
          if (f(x)) {
            Cons(x, go(xs))
          } else {
            go(xs)
          }
        }
      }
    }

    go(as)
  }

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def go(bs: List[A]): List[B] = {
      bs match {
        case Nil => Nil
        case Cons(x: A, xs: List[A]) => {
          val fxs = f(x)
          append(fxs, go(xs))
        }
      }
    }

    go(as)
  }

  // exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => {
      if (f(a)) {
        Cons(a, Nil)
      } else {
        Nil
      }
    })
  }

  // exercise 3.23
  def zipWith[A, B](as: List[A], bs: List[B]): List[(A, B)] = {
    (as, bs) match {
      case (Nil, Nil) => Nil
      case (Cons(x: A, xs: List[A]), Cons(y: B, ys: List[B])) => Cons((x, y), zipWith(xs, ys): List[(A, B)])
    }
  }

  // exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(head1, tail1) =>
        val res = sub match {
          case Nil => true
          case Cons(head2, tail2) =>
            if (head1 == head2) {
              hasSubsequence(tail1, tail2)
            } else {
              false
            }
        }
        if (!res) hasSubsequence(tail1, sub)
        else true
    }
  }
}

object ListTest extends App {

  // exercise 3.6
  val l1 = List(1, 2, 3, 4)
  println("exercise 3.6")
  println(List.init(l1))

  // exercise 3.7
  val l2 = List(0.0, 2, 3, 4)
  println("exercise 3.7")
  println(List.product2(l2))

  // exercise 3.9
  val l3 = List(0.0, 2, 3, 4)
  println("exercise 3.9")
  println(List.length(l3))

  // exercise 3.9
  val l4 = List(0.0, 2, 3, 4, 1)
  println("exercise 3.9")
  println(List.length(l4))

  // exercise 3.10
  val l5 = List(0.0, 2, 3, 4, 1)
  println("exercise 3.10")
  println(List.foldLeft(l5, 0.0)((b, a) => b + a))

  // exercise 3.12
  val l6 = List(0.0, 2, 3, 4, 1)
  println("exercise 3.12")
  println(List.reverse(l6))

  // exercise 3.13
  val l7 = List(0.0, 2, 3, 4, 1)
  println("exercise 3.13")
  println(List.foldRight2(l7, 0.0)((b, a) => b + a))

  // exercise 3.14
  val l8 = List(0.111, 44, 3, 9, 9)
  val l9 = List(0.0, 2, 3, 4, 1)
  println("exercise 3.14")
  println(List.append2(l8, l9))

  // exercise 3.15
  val l10 = List(0.111, 44, 3, 9, 9)
  val l11 = List(0.0, 2, 3, 4, 1)
  val l12 = List(l10, l11)
  println("exercise 3.15")
  println(List.flatMap(l12))

  // exercise 3.16
  val l13 = List(0, 2, 3, 4, 1)
  println("exercise 3.16")
  println(List.incrementList(l13))

  // exercise 3.19
  val l14 = List(0, 2, 3, 4, 1)
  println("exercise 3.19")
  println(List.filter(l14)(_ % 2 == 1))

  // exercise 3.20
  val l15 = List(0, 2, 3, 4, 1)
  println("exercise 3.20")
  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

  // exercise 3.21
  val l16 = List(0, 2, 3, 4, 1)
  println("exercise 3.21")
  println(List.filter(l16)(_ % 2 == 1))

  // exercise 3.23
  println("exercise 3.23")
  println(List.zipWith(List(1, 2, 3), List(4, 5, 6)))

  // exercise 3.24
  println("exercise 3.24")
  println(List.hasSubsequence(List(1,2,3,4), List(2,3)))
  println(List.hasSubsequence(List(1,2,3,4), List(2,5)))
}