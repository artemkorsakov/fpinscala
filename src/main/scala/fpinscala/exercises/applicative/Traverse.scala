package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.state.State
import fpinscala.answers.monoids.{Foldable, Monoid}
import Applicative.Const
import fpinscala.exercises.monads.Id

trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      fa.traverse[Id, B](a => Id(f(a))).value

    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      fa.traverse[Const[B, _], Nothing](f)

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, b) => ((), f(b, a)))(1)

    override def toList: List[A] =
      ???

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a =>
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield b
      ).run(s)

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, as) => (as.head, as.tail))(0)

    def fuse[M[_], N[_], B](f: A => M[B], g: A => N[B])(using
        m: Applicative[M],
        n: Applicative[N]
    ): (M[F[B]], N[F[B]]) =
      fa.traverse[[X] =>> (M[X], N[X]), B](a => (f(a), g(a)))(using m.product(n))

  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
    extension [A](fga: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        self.traverse(fga)(ga => ga.traverse(f))

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        as match
          case Nil => summon[Applicative[G]].unit(List.empty[B])
          case h :: t =>
            summon[Applicative[G]].map2(f(h))(t.traverse(f))(_ :: _)

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        oa match
          case None    => summon[Applicative[G]].unit(None)
          case Some(a) => summon[Applicative[G]].map(f(a))(b => Some(b))

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        val gHead: G[B] = f(ta.head)
        val gTail: G[List[Tree[B]]] =
          ta.tail.traverse(treeA => treeA.traverse(f))
        summon[Applicative[G]].map2(gHead)(gTail)((head, tail) =>
          Tree(head, tail)
        )

  given mapTraverse[K]: Traverse[Map[K, _]] with
    extension [A](m: Map[K, A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
        val g = summon[Applicative[G]]
        m.foldLeft(g.unit(Map.empty[K, B])):
          case (gMapKB, (key -> a)) =>
            gMapKB.map2(f(a))((mapKB, b) => mapKB + (key -> b))
