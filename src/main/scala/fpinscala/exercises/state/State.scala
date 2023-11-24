package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    val next =
      if i == Int.MaxValue then 0.0
      else i.toDouble / Int.MaxValue
    (next, r)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d0, r0) = double(rng)
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    ((d0, d1, d2), r2)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then (List.empty, rng)
    else
      val (i, rng1) = rng.nextInt
      val (prev, rng2) = ints(count - 1)(rng1)
      (i :: prev, rng2)

  def double2(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd =>
      val (a, rnd1) = ra(rnd)
      val (b, rnd2) = rb(rnd1)
      (f(a, b), rnd2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs match
      case Nil    => unit(Nil)
      case h :: t => map2(h, sequence(t))(_ :: _)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng1) = r(rng)
      f(a)(rng1)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, s1) = run(s)
        (f(a), s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s0 =>
        val (a, s1) = run(s0)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s0 =>
        val (a, s1) = run(s0)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
    list.foldLeft(unit[S, List[A]](List.empty[A])) { case (acc, a) =>
      a.map2(acc)(_ :: _)
    }

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(input => State.modify(update(input)))
      s <- State.get
    yield (s.coins, s.candies)

  private val update = (i: Input) =>
    (s: Machine) =>
      (i, s) match
        case (_, Machine(_, 0, _))              => s
        case (Input.Coin, Machine(false, _, _)) => s
        case (Input.Turn, Machine(true, _, _))  => s
        case (Input.Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Input.Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
