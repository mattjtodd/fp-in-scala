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

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???
//    a.flatMap(_ => b).flatMap(_ => Some(f(a, b)))
//  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((next, prev) => map2(next, prev)(_::_))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
