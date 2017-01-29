package chapter4

/**
  * 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = flatMap(value => Right(f(value)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case either @ Left(_) => either
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def map2NoFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(aa => b.flatMap(bb => Right(f(aa, bb))))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((next, prev) => next.map2(prev)(_ :: _))
  }

  def sequenceTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(value => value)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((next, prev) => f(next).map2(prev)(_ :: _))
  }
}

