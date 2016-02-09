package chapter2

import chapter2.Chapter2._

class Chapter2$Test extends UnitSpec {

  "Fib 10" should "be 55" in {
    assert(fib(10) == 55)
  }

  "Fib 0" should "be 1" in {
    assert(fib(0) == 1)
  }

  "Fib 1" should "be 1" in {
    assert(fib(1) == 1)
  }

  "Is sorted" should "be false" in {
    assert(!isSorted(Array(1, 5, 3), (one: Int, two: Int) => one < two))
  }

  "Is sorted" should "be true" in {
    assert(isSorted(Array(10, 50, 300), (one: Int, two: Int) => one < two))
  }

  "When Integer Add Function Curried apply" should "be ten" in {
    val addCurry = curry((a: Int, b: Int) => a + b)
    val addFive = addCurry(5)

    assert(addFive(5) == 10)
  }
}
