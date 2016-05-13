package chapter2

import chapter2.Chapter2._

class Chapter2$Test extends UnitSpec {

  // 2.1 - Fib

  "Fib 10" should "be 55" in {
    assert(fib(10) == 55)
  }

  "Fib 0" should "be 1" in {
    assert(fib(0) == 1)
  }

  "Fib 1" should "be 1" in {
    assert(fib(1) == 1)
  }

  // 2.2 - Is Sorted

  "Is sorted" should "be false" in {
    assert(!isSorted(Array(1, 5, 3), (one: Int, two: Int) => one < two))
  }

  "Is sorted" should "be true" in {
    assert(isSorted(Array(10, 50, 300), (one: Int, two: Int) => one < two))
  }

  // 2.3 - Curry

  "When add curried with 5 and 5 supplied" should "be ten" in {
    val addCurry = curry((a: Int, b: Int) => a + b)
    val addFive = addCurry(5)

    assert(addFive(5) == 10)
  }

  // 2.3 - UnCurry

  "When add uncurried with A + B + C 10" should "be 30" in {
    val curried = (a: Int) => (b: Int) => (c: Int) => a + b + c

    val uncurried = unCurry(curried)

    assert(uncurried(10, 10)(10) == 30)
  }

}
