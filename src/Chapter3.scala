import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader
import fpinscala.datastructures.List._
import fpinscala.datastructures.{Cons, List, Nil}

import scala.annotation.tailrec

/**
 * Created by utuba on 05/04/15.
 */
object Chapter3 {
  val question1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def main(args: Array[String]) {
    // question 1 - the value is three because the third case is the first match
    assert(3 == question1)

    // questions 2 - implement tail for List
    println(tail(List(1, 2, 3, 4, 5)))

    // question 3 - implement drop
    println(drop(List(1, 2, 3, 4, 5), 3))

    // question 4 - dropwhile
    println(dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a < 3))

    // question 5 - setHead
    println(setHead(List(1, 2, 3, 4), 0))

    // question 6 - init
    println(init(List(1, 2, 3, 4, 5, 6)))

    // question 7 - Can product implemented using foldRight immediately halt the recursion and return 0.0 if it encounters a 0.0?
    // answer - no, the function passed in cannot control the flow of execution

    // question 8 - pass in Nil or Cons to the
    println(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))

    // question 9 - Compute the length of a list using foldRight.
    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, acc) => acc + 1)

    println(length(List(1, 2, 3, 4)));

    // question 10 implement fold left to be tail recursive
    @tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
    println(foldLeft(List(1, 2, 3, 4), 0)(
      (x, xs) => {
        println("dd34d3" + x + " " + xs)
        x * xs
      }))

    // question 11 implement sum, product and count using foldLeft
    def sumFl(l: List[Int]) =
      foldLeft(l, 0)(_ + _)

    def productFl(l: List[Int]) =
      foldLeft(l, 1)(_ * _)

    def lengthFl[A](l: List[A]): Int =
      foldLeft(l, 0)((acc, _) => acc + 1)

    println(sumFl(List(1, 2, 3, 4)))
    println(productFl(List(1, 2, 3, 4)))
    println(lengthFl(List(1, 2, 3)))

    // question 12 - Write a function that returns the reverse of a list (so given List(1,2,3) it returns List(3,2,1)).
    // See if you can write it using a fold.
    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, Nil:List[A])((x, xs) => Cons(xs, x))
    }
    println(reverse(List(1, 2, 3)))

    // question 13 - Can you write foldLeft in terms of foldRight? How about the other way around?
    def foldLeftFr[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
      foldRight(l, z)((x, xs) => f(xs, x))
    }

    println(foldLeftFr(List(1, 2, 3), 0)(_ + _))

    def foldRightFl[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(l, z)((x, xs) => f(xs, x))
    }

    println(foldRightFl(List(1, 2, 3), 0)(_ + _))

    // question 14 - Implement append in terms of either foldLeft or foldRight.
    def append[A](l: List[A], app:List[A]) =
      foldRight(l, app)(Cons(_, _))

    println(append(List(1, 2, 3), List(4)))

    // question 15 - Write a function that concatenates a list of lists into a single list. Its runtime should be
    // linear in the total length of all lists. Try to use functions we have already defined.

    def concat[A](l: List[List[A]]): List[A] = {
      foldLeft(l, Nil:List[A])(append)
    }

    println(concat(List(List(1, 2), List(3, 4), List(5, 6), List(7), List())));

    // question 16 - Write a function that transforms a list of integers by adding 1 to each element.
    // (pure function only!!!)
    def addOne(l:List[Int]) =
      foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

    println(addOne(List(1, 2, 3, 4)))

    // question 17 - Write a function that turns each value in a List[Double] into a String.
    def asString(l:List[Double]): List[String] =
      foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

    println(asString(List(1, 2, 3, 4, 5)))

    // question 18 - Write a function map, that generalizes modifying each element in a list while maintaining the
    // structure of the list
    def map[A, B](l:List[A], f:A => B): List[B] =
      foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

    println(map[String, Int](List("a", "aa", "aaa", "aaaa"), x => x.length))

    // question 19 - Write a function filter that removes elements from a list unless they satisfy a given predicate.
    // Use it to remote all odd numbers from a List[Int].
    def filter[A](l:List[A], f:A => Boolean): List[A] = l match {
      case Cons(x, xs) if f(x) => Cons(x, filter(xs, f))
      case Cons(_, xs) => filter(xs, f)
      case Nil => l
    }

    def filterV2[A](l:List[A], f:A => Boolean): List[A] =
      foldRight(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    println(filter[Int](List(1, 2, 3, 4), x => x == 3))
    println(filterV2[Int](List(1, 2, 3, 4), x => x == 3))

    // question 20 - Write a function flatMap, that works like map except that the function given will return a list
    // instead of a single result, and that list should be inserted into the final resulting list.
//    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] {
//
//    }
  }
}
