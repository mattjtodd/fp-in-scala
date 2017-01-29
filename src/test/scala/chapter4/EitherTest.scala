package chapter4

import org.scalatest._
import chapter4.Either._

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

  "4.6 map2 with Right to Right" should " be Right" in  {
    assert(Right(10).map2(Right(10))(_ + _) == Right(20))
  }

  "4.6 map2 with Right to Left" should " be Left" in  {
    assert(Right(10).map2(Left("Failed"))(_ + _) == Left("Failed"))
  }

  "4.7 sequence all right list" should "be list of Right" in  {
    assert(sequence(List(Right(1), Right(2), Right(3))) ==  Right(List(1, 2, 3)))
  }

  "4.7 sequence with Left" should "be Left and first failure" in  {
    assert(sequence(List(Right(1), Left("Failed1"), Left("Failed2"))) ==  Left("Failed1"))
  }

  "4.7 traverse with all Right" should "be Right" in  {
    assert(traverse(List(1, 2, 3, 4))(Right(_)) ==  Right(List(1, 2, 3, 4)))
  }

  "4.7 traverse with a Left on greater than two" should "be Left" in  {
    assert(traverse(List(1, 2, 3, 4))(value => if (value >= 2) Left("GTOE2:" + value) else Right(value)) ==  Left("GTOE2:2"))
  }
}
