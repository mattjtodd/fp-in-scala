package chapter4

object Chapter4 {

  sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = {
        flatMap(value => Some(f(value)))
      }

      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(value) => f(value)
      }

      def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(value) => value
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this map (Some(_)) getOrElse ob
      }

      def filter(f: A => Boolean): Option[A] = {
        flatMap(value => if (f(value)) this else None)
      }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  /**
    * 4.3 Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too. Here is its signature:
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(_a => b.flatMap(_b => Some(f(_a, _b))))
  }

  /**
    * 4.4 Write a function sequence that combines a list of Options into one Option containing a list of all the
    * Some values in the original list. If the original list contains None even once, the result of the function
    * should be None; otherwise the result should be Some with a list of all the values.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((next, prev) => map2(next, prev)(_::_))
  }

  /**
    * 4.5 Implement this function. It’s straightforward to do using map and sequence, but try for a more efficient
    * implementation that only looks at the list once.
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((next, prev) => map2(f(next), prev)(_::_))
  }

  /**
    * 4.5b In fact, implement sequence in terms of traverse.
    */
  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(value => value)
  }
}
