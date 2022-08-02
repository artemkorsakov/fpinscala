package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    val m = if n == Int.MinValue then 0 else math.abs(n)
    (m, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    val m = if n == Int.MaxValue then 0.0 else n.toDouble / Int.MaxValue
    (m, rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), rng3) = intDouble(rng)
    ((d, n), rng3)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldLeft((List.empty[Int], rng)) { case ((list, rng), _) =>
      val (n, rng2) = rng.nextInt
      (n :: list, rng2)
    }

  val _double: Rand[Double] =
    map(nonNegativeInt)(n => if n == Int.MaxValue then 0.0 else n.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft(unit(List.empty[A])) { case (listR, ra) => map2(ra, listR)(_ :: _) }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rnd => {
      val (a, rnd2) = r(rnd)
      f(a)(rnd2)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s1 =>
        val (a, s2) = underlying(s1)
        (f(a), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s1 =>
        val (a, s2) = underlying(s1)
        f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  private lazy val update: Input => Machine => Machine = (i: Input) =>
    (s: Machine) =>
      (i, s) match
        case (_, Machine(_, 0, _))              => s
        case (Input.Coin, Machine(false, _, _)) => s
        case (Input.Turn, Machine(true, _, _))  => s
        case (Input.Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Input.Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
