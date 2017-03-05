package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /**
    * 5.1 Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in
    * the REPL. You can convert to the regular List type in the standard library.
    */
  def toList: List[A] = {
    @tailrec
    def go (stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(head, tail) => go(tail(), head() +: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /**
    * 5.2 Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the
    * first n elements of a Stream.
    */
  def take(number: Int): Stream[A] = this match {
    case Cons(head, tail) if number > 1 => Stream.cons(head(), tail().take(number -1))
    case Cons(head, _) if number == 1 => Stream.cons(head(), Empty)
    case _ => Empty
  }

  def drop(number: Int): Stream[A] = this match {
    case Cons(_, tail) if number > 0 => tail.apply().drop(number-1)
    case _ => this
  }

  /**
    * 5.3 Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
