package chapter5

import org.scalatest._

class StreamTest extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  "5.1 toList with Empty " should " be List()" in  {
    assert(Empty.toList == List.empty)
  }

  "5.1 toList with Stream(1, 2, 3) " should " be List(1, 2, 3)" in  {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  "5.2 take(2) with Stream(1, 2, 3) toList " should " be List(1, 2)" in  {
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
  }

  "5.2 take(1) with Stream(1, 2, 3) toList " should " be List(1)" in  {
    assert(Stream(1, 2, 3).take(1).toList == List(1))
  }

  "5.2 drop(2) with Stream(1, 2, 3) toList " should " be List(3)" in  {
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
  }

  "5.3 takeWhile with Stream(1, 2, 3)(_ <3) " should "be List(1, 2)" in {
    assert(Stream(1, 2, 3).takeWhile(_ < 2).toList == List(1, 2))
  }
}
