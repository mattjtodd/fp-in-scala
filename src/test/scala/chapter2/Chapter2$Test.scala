package chapter2

class Chapter2$Test extends UnitSpec {

  "Fib 10" should "be 55" in {
    assert(Chapter2.fib(10) == 55)
  }

  "Fib 0" should "be 1" in {
    assert(Chapter2.fib(0) == 1)
  }

  "Fib 1" should "be 1" in {
    assert(Chapter2.fib(1) == 1)
  }
}
