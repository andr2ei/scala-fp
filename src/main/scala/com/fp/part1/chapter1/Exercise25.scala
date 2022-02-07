package scala.com.fp.part1.chapter1

object Exercise25 extends App {

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
