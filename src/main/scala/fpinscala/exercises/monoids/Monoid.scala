package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

import scala.annotation.tailrec

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String): String = a1 + a2
    val empty: String = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val empty: List[A] = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    val empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    val empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    val empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty: A = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
    val empty: A => A = identity

  import fpinscala.exercises.testing.{Gen, Prop}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen) { case x ** y ** z =>
        m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))
      }
      .tag("associativity")
    val identity = Prop
      .forAll(gen) { x =>
        m.combine(x, m.empty) == x &&
        m.combine(m.empty, x) == x
      }
      .tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then m.empty
    else if as.length == 1 then f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(x: Par[A], y: Par[A]): Par[A] = x.map2(y)(m.combine)
    val empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap(bs => foldMapV(bs, par(m))(b => Par.lazyUnit(b)))

  private case class Interval(ordered: Boolean, min: Int, max: Int)

  private val orderedMonoid: Monoid[Option[Interval]] = new:
    def combine(oa1: Option[Interval], oa2: Option[Interval]): Option[Interval] =
      (oa1, oa2) match
        case (Some(a1), Some(a2)) => Some(Interval(a1.ordered && a2.ordered && a1.max <= a2.min, a1.min, a2.max))
        case (x, y)               => x.orElse(y)
    val empty: Option[Interval] = None

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => Some(Interval(true, i, i))).forall(_.ordered)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)): (A, B) = ???
    val empty: (A, B) = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B): A => B = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] = ???
    val empty: Map[K, V] = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
