package chapter2

import scala.annotation.tailrec

object Chapter2 {

  /**
    * Exercise 2.1
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(prev2: Int, prev1: Int, count: Int): Int = {
      if (count <= 0) prev2 else go(prev1, prev1 + prev2, count-1)
    }
    if (n <= 2) 1 else go(0, 1, n)
  }

  /**
    * Exercise 2.2
    */
  def isSorted[A] (as: Array[A], orderd: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, result: Boolean): Boolean = {
      if (!result || n >= as.length) result else loop(n + 1, orderd.apply(as(n-1), as(n)))
    }
    loop(1, true)
  }

  /**
    * Exercise 2.3
    */
  def curry[A, B, C] (f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  /**
    * Exercise 2.4
    */
  def unCurry[A, B, C] (f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Exercise 2.5
    */
  def compose[A, B, C] (f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
