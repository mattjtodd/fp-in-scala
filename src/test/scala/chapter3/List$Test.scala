package chapter3

import chapter3.Chapter3.List._
import chapter3.Chapter3.{Branch, Cons, Leaf, List, Nil, Tree}
import chapter3.Chapter3.Tree._
import util.UnitSpec

class List$Test extends UnitSpec {

  "Nil List tail" should "be Nil" in {
    assert(tail(Nil) == Nil)
  }

  "Tail" should "be Tail" in {
    val expected = Cons("Tail")
    assert(tail(Cons("Head", expected)) == expected)
  }

  "Head" should "be New" in {
    assert(setHead("New", Cons("Old")) == Cons("New"))
  }


  "3.4 Drop with single elemnts" should "drop to Nil" in {
    assert(drop(Cons(""), 1) == Nil)
  }

  "3.4 Drop with Nil" should "drop to Nil" in {
    assert(drop(Nil, 1) == Nil)
  }

  "3.5 Drop with Nil" should "be Nil" in {
    assert(dropWhile(Nil, (value: String) => true) == Nil)
  }

  "3.5 Drop with always true" should "be Nil" in {
    assert(dropWhile(Cons("1", Cons("2")), (value: String) => true) == Nil)
  }

  "3.5 Drop with just once" should "be Nil" in {
    assert(dropWhile(Cons(1, Cons(2)), (value: Int) => value == 1) == Cons(2))
  }

  "3.6 Init with single value should be " should "be Nil" in {
    assert(init(Cons(1, Nil)) == Nil)
  }

  "3.6 Init with 1,2,3 " should "be 1,2" in {
    assert(init(List(1, 2, 3)) == List(1, 2))
  }

  "3.8 foldRight over with empty list to Cons" should "return the same list" in {
    /**
      * 3.8 See what happens when you pass Nil and Cons themselves to foldRight, like this:
      *   foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
      * What do you think this says about the relationship between foldRight and the data constructors of List?
      */
    assert(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == List(1, 2, 3))
  }

  "3.9 Given 1,1,1,1,1 length" should "be 5" in {
    assert(Chapter3.List.length(List(1, 1, 1, 1, 1)) == 5)
  }

  "3.10 foldLeft with sum Given 1,1,1,5" should "be 8" in {
    assert(foldLeft(List(1, 1, 1, 5), 0)((prev, next) => prev + next) == 8)
  }

  "3.11 Given 1,2,3,4,5 sum" should "be 15" in {
    assert(sumFoldLeft(List(1, 2, 3, 4, 5)) == 15)
  }

  "3.11 Given 1,2,3,4,5 product" should "be 120" in {
    assert(productFoldLeft(List(1, 2, 3, 4, 5)) == 120)
  }

  "3.11 Given 1,1,1,1,1 length" should "be 5" in {
    assert(lengthFoldLeft(List(1, 1, 1, 1, 1)) == 5)
  }

  "3.12 Given 1,2,3 reversed" should "be 3,2,1" in {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  "3.14 Given 1,2,3 appendFoldLeft with 4,5,6" should "be 1,2,3,4,5,6" in {
    assert(appendFoldLeft(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  "3.14 Given 1,2,3 appendFoldRight with 4,5,6" should "be 1,2,3,4,5,6" in {
    assert(appendFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  "3.15 Given [[1,2,3],[4,5,6],[7,8,9]] concat " should "be [1,2,3,4,5,6,7,8,9]" in {
    assert(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "3.16 Given [1,2,3,4,5] addOne " should "be [2,3,4,5,6]" in {
    assert(addOne(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5, 6))
  }

  "3.17 Given [1.0, 2.0, 3.0, 4.0, 5.0] addOne " should "be ['1.0', '2.0', '3.0', '4.0', '5.0']" in {
    assert(doubleToString(List(1.0, 2.0, 3.0, 4.0, 5.0)) == List("1.0", "2.0", "3.0", "4.0", "5.0"))
  }

  "3.18 Given [1,2,3,4,5] map with add one " should "be [2,3,4,5,6]" in {
    assert(map(List(1, 2, 3, 4, 5))(_ + 1) == List(2, 3, 4, 5, 6)
    )
  }

  "3.19 Given [1,2,3,4,5] filter with even" should "be [2,4]" in {
    assert(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4)
    )
  }

  "3.20 Given [1,2,3,4,5] flatMap with duplicate" should "be [1,1,2,2,3,3,4,4,5,5]" in {
    assert(flatMap(List(1, 2, 3, 4, 5))(x => List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
    )
  }

  "3.21 Given [1,2,3,4,5] filterWithFlatMap with even" should "be [2,4]" in {
    assert(filterWithFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4)
    )
  }

  "3.22 Given [1,2,3] and [4,5,6] addLists" should "be [5,7,9]" in {
    assert(addLists(List(1, 2, 3, 4), List(4, 5, 6)) == List(5, 7, 9))
  }

  "3.23 Given [1,2,3] and [4,5,6] zipWith add" should "be [5,7,9]" in {
    assert(zipWith(List(1, 2, 3, 4), List(4, 5, 6))(_ + _) == List(5, 7, 9))
  }

  "3.24 Given [1,2,3,4,5] and [3,4] hasSubsequence" should "be true" in {
    assert(hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)))
  }

  "3.24 Given [1,2,3,4,5] and [7] hasSubsequence" should "be true" in {
    assert(!hasSubsequence(List(1, 2, 3, 4, 5), List(7)))
  }

  "3.24 Given [1,2,4,2,3,4,5] and [2,3] hasSubsequence" should "be true" in {
    assert(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)))
  }

  "3.25 Given Branch(Leaf(1), Leaf(2)) size" should "be 3" in {
    assert(nodeCount(Branch(Leaf(1), Leaf(2))) == 3)
  }

  "3.26 Given Branch(Leaf(1), Leaf(5)) maxLeaf" should "be 5" in {
    assert(maxLeaf(Branch(Leaf(1), Leaf(5))) == 5)
  }

  "3.27 Given Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))) depth" should "be 4" in {
    assert(depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) == 4)
  }

  "3.27 Given Branch(Leaf(1), Leaf(2)) depth" should "be 2" in {
    assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
  }

  "3.28 Given Branch(Leaf(1), Leaf(2)) mapTree with multiply by two" should "be Branch(Leaf(2), Leaf(4))" in {
    assert(mapTree(Branch(Leaf(1), Leaf(2)))(_ * 2) == Branch(Leaf(2), Leaf(4)))
  }

  "3.29 Given Branch(Leaf(1), Leaf(2)) sizeViaFoldTree" should "be 3" in {
    assert(sizeViaFoldTree(Branch(Leaf(1), Leaf(2))) == 3)
  }

  "3.29 Given Branch(Leaf(1), Leaf(5)) maxLeafViaFold" should "be 5" in {
    assert(maxLeafViaFold(Branch(Leaf(1), Leaf(5))) == 5)
  }

  "3.29 Given Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))) depthViaFold" should "be 4" in {
    assert(depthViaFold(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) == 4)
  }

  "3.29 Given Branch(Leaf(1), Leaf(2)) depthViaFold" should "be 2" in {
    assert(depthViaFold(Branch(Leaf(1), Leaf(2))) == 2)
  }
}
