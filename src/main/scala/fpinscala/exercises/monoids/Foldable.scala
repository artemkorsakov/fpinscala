package fpinscala.exercises.monoids

import scala.annotation.tailrec

trait Foldable[F[_]]:
  import Monoid.{dual, endoMonoid}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      val b2b = foldMap[B => B](f.curried)(using endoMonoid[B])
      b2b(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      val b2b = foldMap[B => B](a => b => f(b, a))(using dual(endoMonoid[B]))
      b2b(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      foldRight(mb.empty)((a, b) => mb.combine(f(a), b))

    def combineAll(using ma: Monoid[A]): A =
      foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      foldRight(List.empty[A])(_ :: _)

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Branch, Leaf}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B = as match
        case Leaf(a) => f(a, acc)
        case Branch(left, right) =>
          val rAcc = right.foldRight(acc)(f)
          left.foldRight(rAcc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B = as match
        case Leaf(a) => f(acc, a)
        case Branch(left, right) =>
          val lAcc = left.foldLeft(acc)(f)
          right.foldLeft(lAcc)(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Leaf(a) => f(a)
        case Branch(left, right) =>
          mb.combine(left.foldMap(f), right.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B = as match
        case None    => acc
        case Some(a) => f(a, acc)
      override def foldLeft[B](acc: B)(f: (B, A) => B): B = as match
        case None    => acc
        case Some(a) => f(acc, a)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case None    => mb.empty
        case Some(a) => f(a)
