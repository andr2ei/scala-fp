package scala.com.fp.part1.chapter1

object Exercise21 extends App {

  print(fib(6))

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, i: Int): Int = {
      if (n == i) return n
      go(b, a + b, i+1)
    }

    go(0, 1, 0)
  }

}
