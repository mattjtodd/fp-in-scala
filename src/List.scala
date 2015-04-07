/**
 * Created by utuba on 04/04/15.
 */
package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  def tail[A](list:List[A]): List[A] = list match{
    case Cons(_, xs) => xs
  }

  @tailrec
  def drop[A](list:List[A], number:Int): List[A] = list match {
    case Cons(_, xs) if number >= 1 =>
        drop(xs, number-1)
    case _ => list
  }

  @tailrec
  def dropWhile[A](list:List[A], func: A => Boolean): List[A] = list match {
    case Cons(x, xs) if func(x) =>
        dropWhile(xs, func)
    case _ => list

  }

  def setHead[A](list:List[A], head:A): List[A] = {
    Cons(head, list)
  }

  def init[A](list:List[A]): List[A] = list match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}