package chapter4

import org.scalatest._

class EitherTest  extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {

  "4.6 flatMap with Right('Hello') to length" should " be Right(5)" in  {
    assert(Right("Hello").flatMap(value => Right(value.length)) == Right(5))
  }

  "4.6 map with Right('Hello') to length" should " be Right(5)" in  {
    assert(Right("Hello").map(_.length) == Right(5))
  }

  "4.6 orElse with Failed" should " be Right('Passed')" in  {
    assert(Left("Falied").orElse(Right("Passed")) == Right("Passed"))
  }

  "4.6 map2 with Passed" should " be Failed('Failed')" in  {
    assert(Right(10).map2(Right(10))(_ + _) == Right(20))
  }
}
