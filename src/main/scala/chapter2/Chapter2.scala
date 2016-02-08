package chapter2

import scala.annotation.tailrec

/**
 * Created by utuba on 01/04/15.
 */
object Chapter2 {

  def nthFactorial(n: Int): Int = {
    @tailrec
    def go(one: Int, two: Int, count: Int): Int =
      if (n == 0)
        0
      else if (count == n)
        two
      else go(two, one + two, count + 1)
    go(0, 1, 1)
  }

  def mrPrinty(value: Int, f: Int => Int): Unit = {
    println(s"$value is ${f(value)}")
  }

  def isSorted[A](arr: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(prev:A, next:Array[A]): Boolean = {
      if (next.headOption.isEmpty)
        true
      else if (!gt(prev, next.head))
        false
      else
        go(next.head, next.tail)
    }
    if (arr.isEmpty)
      true
    else
      go(arr.head, arr.tail)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    b:B => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a:A => b => f(a, b)
  }

  def unCurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a:A => f(g(a))
  }

  def main(args: Array[String]) {

    mrPrinty(10, nthFactorial)
    def prod(x:Int) = x * x
    mrPrinty(10, value => value * value)

    println(isSorted(Array(), (x:Int, y:Int) => x <= y))
    println(isSorted(Array(3), (x:Int, y:Int) => x <= y))
    println(isSorted(Array(3, 4, 5), (x:Int, y:Int) => x <= y))
    println(isSorted(Array(3, 40, 5), (x:Int, y:Int) => x <= y))

    val oneHundredTimes = partial1(100, (a:Int, b:Int) => a*b)

    println(oneHundredTimes(100))

    val concat = curry((s:String, y:String) => s+y)
    val hello = concat("hello")
    println(hello("Mince"))

    val uncurried = unCurry(concat)
    println(uncurried("hello", "mince"))

  }
}
