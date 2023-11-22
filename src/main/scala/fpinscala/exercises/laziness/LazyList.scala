package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def toList: List[A] = this match
    case LazyList.Empty      => Nil
    case LazyList.Cons(h, t) => h() :: t().toList

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    if n <= 0 then LazyList.Empty
    else
      this match
        case LazyList.Empty => LazyList.Empty
        case LazyList.Cons(h, t) =>
          LazyList.Cons(() => h(), () => t().take(n - 1))

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(() => h(), () => t().takeWhile(p))
    case _                    => LazyList.Empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = this match
    case Empty      => Empty
    case Cons(h, t) => Cons(() => f(h()), () => t().map(f))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def append[B >: A](that: LazyList[B]): LazyList[B] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, bs) => f(a).append(bs))

  def startsWith[B](s: LazyList[B]): Boolean = ???

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    Cons(() => a, () => continually(a))

  def from(n: Int): LazyList[Int] =
    Cons(() => n, () => from(n + 1))

  lazy val fibs: LazyList[Int] =
    def loop(first: Int, second: Int): LazyList[Int] =
      Cons(() => first, () => loop(second, first + second))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(
    state
  ) match
    case None            => empty
    case Some((a, next)) => cons(a, unfold(next)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (f, s) =>
      Some((f, (s, f + s)))
    }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n): n =>
      Some((n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a): a =>
      Some((a, a))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(()): _ =>
      Some((1, ()))
