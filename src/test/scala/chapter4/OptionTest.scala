package chapter4

import chapter4.Chapter4.{Option, Some}

import org.scalatest._

/**
  * Created by utuba on 25/12/2016.
  */
class OptionTest  extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {

  "map with Some(4) add 4 " should "Some(8)" in  {
    assert(Some(4).map(_ + 4) == Some(8))
  }
}
