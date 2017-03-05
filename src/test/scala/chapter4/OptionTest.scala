package chapter4

import chapter4.Option._

import org.scalatest._

class OptionTest extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  "4.0 map with Some(4) add 4 " should "Some(8)" in  {
    assert(Some(4).map(_ + 4) == Some(8))
  }

  "4.3 map2 None, None " should "be None" in  {
    assert(map2(None, None)((a, _) => a) == None)
  }

  "4.3 map2 Some('Test'), None " should "be None" in  {
    assert(map2(Some("Test"), None)((a, b) => a) == None)
  }

  "4.3 map2 Some('Test1'), Some('Test2') " should "be Some('Test1')" in  {
    assert(map2(Some("Test1"), Some("Test2"))((a, _) => a) == Some("Test1"))
  }

  "4.4 sequence List(None) " should "be None" in  {
    assert(sequence(List(None)) == None)
  }

  "4.4 sequence List(Some('Test')) " should "be Some(List('Test'))" in  {
    assert(sequence(List(Some("Test"))) == Some(List("Test")))
  }

  "4.4 sequence List(Some('Test'), None) " should "be None" in  {
    assert(sequence(List(Some("Test"), None)) == None)
  }

  "4.4 sequence List(Some('Test'), Some('Test')) " should "be Some(List('Test', 'Test'))" in  {
    assert(sequence(List(Some("Test"), Some("Test"))) == Some(List("Test", "Test")))
  }

  "4.5 traverse List(1, 2, 3) with always Some " should "be Some(List(1, 2, 3))" in  {
    assert(traverse(List(1, 2, 3))(Some(_)) == Some(List(1, 2, 3)))
  }

  "4.5 traverse List(1, 2, 3) with always None " should "be None" in  {
    assert(traverse(List(1, 2, 3))(_ => None) == None)
  }

  "4.5 traverse List(1, 2, 3) when >= 2 None " should "be None" in  {
    assert(traverse(List(1, 2, 3))(value => if (value >= 2) None else Some(value)) == None)
  }

  "4.5b sequenceTraverse List(None) " should "be None" in  {
    assert(sequenceTraverse(List(None)) == None)
  }

  "4.5b sequenceTraverse List(Some('Test')) " should "be Some(List('Test'))" in  {
    assert(sequenceTraverse(List(Some("Test"))) == Some(List("Test")))
  }

  "4.5b sequenceTraverse List(Some('Test'), None) " should "be None" in  {
    assert(sequenceTraverse(List(Some("Test"), None)) == None)
  }

  "4.5b sequenceTraverse List(Some('Test'), Some('Test')) " should "be Some(List('Test', 'Test'))" in  {
    assert(sequenceTraverse(List(Some("Test"), Some("Test"))) == Some(List("Test", "Test")))
  }
}
