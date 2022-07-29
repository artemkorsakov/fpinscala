package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => List.empty[A]
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
    if n <= 0 then Empty
    else
      this match
        case Empty      => Empty
        case Cons(h, t) => Cons(h, () => t().take(n - 1))

  def drop(n: Int): LazyList[A] =
    if n <= 0 then this
    else
      this match
        case Empty      => Empty
        case Cons(_, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _                    => Empty

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Some(h())

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, list) => if p(a) then Cons(() => a, () => list) else Empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, list) => Cons(() => f(a), () => list))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, list) => if p(a) then Cons(() => a, () => list) else list)

  def append[B >: A](list: => LazyList[B]): LazyList[B] =
    foldRight(list)((a, list) => Cons(() => a, () => list))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, list) => f(a).append(list))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold(this) {
      case Cons(h, t) if n > 0 => Some((h(), t().takeViaUnfold(n - 1)))
      case _                   => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t().takeWhileViaUnfold(p)))
      case _                    => None
    }

  def zipWith[B >: A](that: LazyList[B])(f: (A, B) => B): LazyList[B] =
    LazyList.unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty)        => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2))        => Some(((None, Some(h2())), (Empty, t2())))
      case _                            => None
    }

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

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(a: Int, b: Int): LazyList[Int] =
      cons(a, go(b, a + b))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None         => Empty
    case Some((a, s)) => Cons(() => a, () => unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold[Int, (Int, Int)]((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold[Int, Int](n)(s => Some((s, s + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold[A, A](a)(s => Some((s, s)))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(1)(s => Some((s, s)))
