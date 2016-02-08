package chapter5

/**
 * A non-stric stream.
 */
/**
 * Companion stream object.
 */
object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def value = None
    }

  /**
   * Creates a new stream object from the non-strict head evaluation and the non-strict tail evaluation.
   *
   * @param head the non-strict head expression
   * @param tail the non-strict tail expression
   * @return the new stream
   */
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val value = Some((head, tail))
    }

  /**
   * Create a stream from the supplied values.
   *
   * @param values the values to create a stream from
   * @return the new stream of the values
   */
  def apply[A](values: A*): Stream[A] = {
    if (values.isEmpty) empty
    else cons(values.head, apply(values.tail: _*))
  }

  trait Stream[+A] {
    def value: Option[(A, Stream[A])]

    def isEmpty: Boolean = value.isEmpty

    def toList: List[A] = value match {
      case Some((head, tail)) => head :: tail.toList
      case None => List()
    }

    def take(n: Int): Stream[A] = value match {
      case Some((head, tail)) => Stream.cons(head, Stream.empty)
      case None => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      value match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h,t) =>
        if (f(h)) cons(h, t)
        else t)

  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def main(args: Array[String]) {
    println(from(0).filter(value => value > 1000000 && value < 1000001).toList);
  }
}
