package scala.com.fp.part1.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
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
      if(i == n) return res
      go(tail(res), i+1)
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
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  // exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) if xs == Cons(x, Nil) => Cons(x, Nil)
      case Cons(x, xs) =>  append(List(x), init(xs))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a: A, b: Int) => {
      1 + b
    })
  }

  // exercise 3.10
}

object ListTest extends App {

  // exercise 3.6
  val l1 = List(1,2,3,4)
  println(List.init(l1))

  // exercise 3.7
  val l2 = List(0.0,2,3,4)
  println(List.product2(l2))

  // exercise 3.9
  val l3 = List(0.0,2,3,4)
  println(List.length(l3))

  // exercise 3.9
  val l4 = List(0.0,2,3,4,1)
  println(List.length(l4))
}