package scala.com.fp.part1.chapter1

object Exercise23 extends App {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a: A => ((b: B) => f(a, b))
  }

}
