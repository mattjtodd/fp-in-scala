package chapter2

import scala.annotation.tailrec

object Chapter2 {
  def fib(n: Int): Int = {

    @tailrec
    def go(prev2: Int, prev1: Int, count: Int): Int = {
      println(count)
      if (count <= 0) {
        prev2
      } else {
        go(prev1, prev1 + prev2, count-1)
      }
    }

    if (n <= 2) {
      1
    } else {
      go(0, 1, n)
    }
  }
}
