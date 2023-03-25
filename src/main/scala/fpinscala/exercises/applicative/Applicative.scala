package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fa.map2(fab)((a, f) => f(a))

  extension [A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((a, b) => (a, b))

    def map3[B, C, D](
        fb: F[B],
        fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[B, C, D, E](
        fb: F[B],
        fc: F[C],
        fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
    override def unit[A](a: => A): (F[A], G[A]) =
      (self.unit(a), G.unit(a))

    override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
      (
        self.apply(fab._1)(fa._1),
        G.apply(fab._2)(fa._2)
      )

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    override def unit[A](a: => A): F[G[A]] =
      self.unit(G.unit(a))

    override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
      val fgab: F[G[A => B] => G[A] => G[B]] =
        self.unit { gab => ga => G.apply(gab)(ga) }
      val fga2gb: F[G[A] => G[B]] = self.apply(fgab)(fab)
      self.apply(fga2gb)(fa)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((key, fv), acc) =>
      fv.map2(acc) { (v, resMap) =>
        resMap + (key -> v)
      }
    }

object Applicative:
  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C): ZipList[C] =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]

  object Validated:
    given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
      def unit[A](a: => A): Validated[E, A] = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
          (fa, fb) match
            case (Valid(a), Valid(b)) => Valid(f(a, b))
            case (Invalid(error1), Invalid(error2)) =>
              val m = summon[Monoid[E]]
              Invalid(m.combine(error1, error2))
            case (_, e @ Invalid(_)) => e
            case (e @ Invalid(_), _) => e

  type Const[A, B] = A

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A]) override def flatMap[B](f: A => Option[B]): Option[B] = oa.flatMap(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]): Either[E, B] =
        eea match
          case Right(value) => f(value)
          case Left(value)  => Left(value)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
