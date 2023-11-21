package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e)  => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case ra @ Right(_) => ra
    case _             => b

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, eb) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _)         => Left(e)
      case (_, Left(ee))        => Left(ee)

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match
      case Nil => Right(List[B]())
      case h :: t =>
        f(h).map2(traverse(t)(f))(_ :: _)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] = (a, b) match
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)
    case (Left(e), _)         => Left(e)
    case (_, Left(e))         => Left(e)

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] = as match
    case Nil => Right(Nil)
    case h :: t =>
      map2All(f(h), traverseAll(t, f), _ :: _)

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] = traverseAll(as, identity)
