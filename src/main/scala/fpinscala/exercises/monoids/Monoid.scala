package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

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
    def combine(x: A => A, y: A => A): A => A = a => y(x(a))
    val empty: A => A = identity

  import fpinscala.exercises.testing.{Prop, Gen}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid)(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length <= 1 then as.headOption.map(f).getOrElse(m.empty)
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)
    val empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap(bs => foldMapV(bs, par(m))(b => Par.lazyUnit(b)))

  opaque type Interval = (Int, Int)

  val orderedMonoid: Monoid[(Boolean, Option[Interval])] = new:
    def combine(
        a1: (Boolean, Option[Interval]),
        a2: (Boolean, Option[Interval])
    ) =
      (a1(1), a2(1)) match
        case (Some((leftMin, leftMax)), Some((rightMin, rightMax))) =>
          (a1(0) && a2(0) && leftMax <= rightMin, Some((leftMin, rightMax)))
        case _ =>
          (a1(0) && a2(0), a1(1).orElse(a2(1)))

    val empty = (true, None)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => (true, Some((i, i))))(0)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    val empty: WC = WC.Stub("")
    def combine(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b))       => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) =>
        WC.Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

  def count(s: String): Int =
    def wc(c: Char): WC =
      if c.isWhitespace then WC.Part("", 0, "")
      else WC.Stub(c.toString)

    def unstub(s: String) = if s.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(s)       => unstub(s)
      case WC.Part(l, w, r) => unstub(l) + w + unstub(r)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)]
  with
    def combine(x: (A, B), y: (A, B)): (A, B) =
      (
        ma.combine(x._1, y._1),
        mb.combine(x._2, y._2)
      )
    val empty: (A, B) = (ma.empty, mb.empty)

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty))
        )

    val empty: Map[K, V] = Map.empty[K, V]

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B): A => B = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as.foldLeft(Map.empty[A, Int]): (acc, a) =>
      acc + (a -> (acc.getOrElse(a, 0) + 1))

end Monoid
