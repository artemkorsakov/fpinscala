package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State
import fpinscala.exercises.monads.Id

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)((f, a) => f(a))

  extension [A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      val fbc = apply[A, B => C](unit(a => f(a, _)))(fa)
      apply[B, C](fbc)(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as match
      case Nil => unit(List.empty[B])
      case h :: t =>
        f(h).map2(traverse(t)(f))(_ :: _)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((a, b) => (a, b))

    def map3[B, C, D](
        fb: F[B],
        fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      val fbcd = apply(unit(f.curried))(fa)
      val fcd = apply(fbcd)(fb)
      apply(fcd)(fc)

    def map4[B, C, D, E](
        fb: F[B],
        fc: F[C],
        fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      val fbcde = apply(unit(f.curried))(fa)
      val fcde = apply(fbcde)(fb)
      val fde = apply(fcde)(fc)
      apply(fde)(fd)

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
    new Applicative[[x] =>> (F[x], G[x])]:
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(
          fa: (F[A], G[A])
      ): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
    new Applicative[[x] =>> F[G[x]]]:
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      extension [A](fga: F[G[A]])
        override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fga)(fgb)(G.map2(_)(_)(f))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(self.unit(Map.empty[K, V])):
      case (resultFMap, (key, fValue)) =>
        resultFMap.map2(fValue)((resultMap, value) =>
          resultMap + (key -> value)
        )

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
      def unit[A](a: => A): Validated[E, A] = Validated.Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(
            f: (A, B) => C
        ): Validated[E, C] =
          (fa, fb) match
            case (Valid(a), Valid(b)) => Valid(f(a, b))
            case (Invalid(e1), Invalid(e2)) =>
              Invalid(summon[Monoid[E]].combine(e1, e2))
            case (e @ Invalid(_), _) => e
            case (_, e @ Invalid(_)) => e

  type Const[A, B] = A

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = oa.flatMap(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]): Either[E, B] =
        eea match
          case Right(a) => f(a)
          case Left(e)  => Left(e)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)

  given Applicative[Id] with
    def unit[A](a: => A): Id[A] = Id(a)

    extension [A](fa: Id[A])
      override def map2[B, C](fb: Id[B])(f: (A, B) => C): Id[C] =
        Id(f(fa.value, fb.value))
