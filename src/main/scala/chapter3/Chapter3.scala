package chapter3

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A] = Nil) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // 3.1 What will be the result of the following match expression?

    //  val x = List(1,2,3,4,5) match {
    //    case Cons(x, Cons(2, Cons(4, _))) => x
    //    case Nil => 42
    //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //    case Cons(h, t) => h + sum(t)
    //    case _ => 101
    //  }

    // 3!

    /**
      * 3.2 Implement the function tail for removing the first element of a List. Note that the function takes constant time.
      * What are different choices you could make in your implementation if the List is Nil?
      * We’ll return to this question in the next chapter.
      *
      * @param list
      * @return the tail of the list or Nil
      */
    def tail[A](list: List[A]): List[A] = list match {
      case Cons(_, tail) => tail
      case _ => Nil
    }

    /**
      * 3.3 Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
      */
    def setHead[A](head: A, list: List[A]): List[A] = {
      Cons(head, tail(list))
    }

    /**
      * 3.4 Generalize tail to the function drop, which removes the first n elements from a list.
      * Note that this function takes time proportional only to the number of elements being dropped—we don’t
      * need to make a copy of the entire List.
      */
    @tailrec
    def drop[A](list: List[A], count: Int): List[A] = count match {
      case 0 => list
      case _ => drop(tail(list), count-1)
    }

    /**
      * 3.5 Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
      */
    def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
      case Cons(head, tail) => if (predicate.apply(head)) dropWhile(tail, predicate) else list
      case _ => Nil
    }

    /**
      * 3.6 Not everything works out so nicely. Implement a function, init, that returns a List consisting of
      * all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
      * Why can’t this function be implemented in constant time like tail?
      */
    def init[A](list: List[A]): List[A] = list match {
      case Nil => throw new IllegalArgumentException("Nil cannot be init")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

    /**
      * 3.7 Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
      * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
      * This is a deeper question that we’ll return to in chapter 5.
      *
      * There is no way to detect from the function definition when to terminate early.
      */
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    /**
      * 3.9 Compute the length of a list using foldRight.
      */
    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, count) => count + 1)
    }

    /**
      * 3.10 Our implementation of foldRight is not tail-recursive and will result in a StackOver- flowError for
      * large lists (we say it’s not stack-safe). Convince yourself that this is the case, and then write another
      * general list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the
      * previous chapter.
      */
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /**
      * 3.11 Write sum, product, and a function to compute the length of a list using foldLeft.
      */
    def sumFoldLeft(as: List[Int]): Int = {
      foldLeft(as, 0)(_ + _)
    }

    def productFoldLeft(as: List[Int]): Int = {
      foldLeft(as, 1)(_ * _)
    }

    def lengthFoldLeft(as: List[Int]): Int = {
      foldLeft(as, 0)((sum, _) => sum + 1)
    }

    /**
      * 3.12 Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
      * See if you can write it using a fold.
      */
    def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]())((acc, head) => Cons(head, acc))

    /**
      * 3.13 Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
      * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
      * which means it works even for large lists without overflow- ing the stack.
      */
    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

    /**
      * 3.14 Implement append in terms of either foldLeft or foldRight.
      */
    def appendFoldLeft[A,B](head: List[A], tail: List[A]): List[A] = {
      foldLeft(reverse(head), tail)((acc, head) => Cons(head, acc))
    }

    def appendFoldRight[A,B](head: List[A], tail: List[A]): List[A] = {
      foldRight(head, tail)((acc, head) => Cons(acc, head))
    }

    /**
      * 3.15 Write a function that concatenates a list of lists into a single list.
      * Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
      */
    def concat[A](lists: List[List[A]]): List[A] = lists match {
      case Nil => Nil
      case Cons(head, Nil) => head
      case Cons(head, tail) => appendFoldRight(head, concat(tail))
    }

    /**
      * 3.16 Write a function that transforms a list of integers by adding 1 to each element.
      * (Reminder: this should be a pure function that returns a new List!)
      */
    def addOne(ints: List[Int]): List[Int] = ints match {
      case Cons(head, tail) => Cons(head + 1, addOne(tail))
      case _ => Nil
    }

    /**
      * 3.17 Write a function that turns each value in a List[Double] into a String.
      * You can use the expression d.toString to convert some d: Double to a String.
      */
    def doubleToString(doubles: List[Double]): List[String] = doubles match {
      case Cons(head, tail) => Cons(head.toString, doubleToString(tail))
      case _ => Nil
    }

    /**
      * 3.18 Write a function map that generalizes modifying each element in a list while maintain-ing the structure of
      * the list.
      */
    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
      case _ => Nil
    }

    /**
      * 3.19 Write a function filter that removes elements from a list unless they satisfy a given predicate.
      * Use it to remove all odd numbers from a List[Int]."
      */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
      case Cons(_, tail) => filter(tail)(f)
      case _ => Nil
    }

    /**
      * 3.20 Write a function flatMap that works like map except that the function given will return a list instead
      * of a single result, and that list should be inserted into the final resulting list.
      */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
      case Cons(head, tail) => appendFoldRight(f(head), flatMap(tail)(f))
      case _ => Nil
    }

    /**
      * 3.21 Use flatMap to implement filter.
      */
    def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(x => if (f(x)) List(x) else Nil)
    }

    /**
      * 3.22 Write a function that accepts two lists and constructs a new list by adding correspond- ing elements.
      * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
      */
    def addLists(left: List[Int], right: List[Int]): List[Int] = {
      def go(leftRight: (List[Int], List[Int])): List[Int] = leftRight match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(lefth, leftt), Cons(righth, rightt)) => Cons(lefth + righth, go(leftt, rightt))
      }
      go(left, right)
    }

    /**
      * 3.23 Generalize the function you just wrote so that it’s not specific to integers or addition.
      * Name your generalized function zipWith.
      */
    def zipWith[A, B](left: List[A], right: List[A])(f: (A, A) => B): List[B] = {
      def go(leftRight: (List[A], List[A])): List[B] = leftRight match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(lefth, leftt), Cons(righth, rightt)) => Cons(f(lefth, righth), go(leftt, rightt))
      }
      go(left, right)
    }

    /**
      * 3.24 implement hasSubsequence for checking whether a List contains another List as a subsequence. For instance,
      * List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
      */
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def go(state: (List[A], List[A])): Boolean = state match {
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(suph, supt), Cons(subh, subt)) => if (suph == subh) go(supt, subt) else go(supt, sub)
      }
      go(sup, sub)
    }
  }

  // The tree trait
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    /**
      * Write a function size that counts the number of nodes (leaves and branches) in a tree.
      */
    def nodeCount[A](tree: Tree[A]): Int = {
      @tailrec
      def go(trees: List[Tree[A]], acc: Int): Int = trees match {
        case Nil => acc
        case Cons(Leaf(_), next) => go(next, acc + 1)
        case Cons(Branch(left, right), next) => go(List.appendFoldLeft(List(left, right), next), acc + 1)
      }
      go(List(tree), 0)
    }

    /**
      * 3.26 Write a function maximum that returns the maximum element in a Tree[Int].
      */
    def maxLeaf(tree: Tree[Int]): Int = {
      @tailrec
      def go(trees: List[Tree[Int]], acc: Int): Int = trees match {
        case Nil => acc
        case Cons(Leaf(value), next) => go(next, acc max value)
        case Cons(Branch(left, right), next) => go(List.appendFoldLeft(List(left, right), next), acc)
      }
      go(List(tree), 0)
    }
  }
}
