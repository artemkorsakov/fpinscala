package fpinscala.exercises.testing

import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import fpinscala.exercises.testing.Gen.*
import fpinscala.exercises.testing.Prop.*
import fpinscala.exercises.testing.Prop.Result.{Falsified, Passed, Proved}

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName
/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed          => false
      case Falsified(_, _) => true
      case Proved          => false

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { case (a, i) =>
        try if f(a) then Passed else Falsified(a.toString, i)
        catch case e: Exception => Falsified(buildMsg(a, e), i)
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("and-left")(max, n, rng) match
          case Passed | Proved => that.tag("and-right")(max, n, rng)
          case x               => x

    def ||(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("or-left")(max, n, rng) match
          // In case of failure, run the other prop.
          case Falsified(msg, _) => that.tag("or-right").tag(msg.string)(max, n, rng)
          case x                 => x

    /* This is rather simplistic - in the event of failure, we simply wrap
     * the failure message with the given message.
     */
    def tag(msg: String): Prop =
      (max, n, rng) =>
        self(max, n, rng) match
          case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
          case x               => x

    def run(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")

  val executor: ExecutorService = Executors.newCachedThreadPool

  def check(p: => Boolean): Prop =
    (_, _, _) => if p then Passed else Falsified("()", 0)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    p.map2(p2)(_ == _)

end Prop

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)

    def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(that)(f)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, self)

    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def list: SGen[List[A]] =
      n => listOfN(n)

    def nonEmptyList: SGen[List[A]] =
      n => listOfN(n.max(1))

    def unsized: SGen[A] = _ => self

    @targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] =
      map2(gb)((_, _))

    def next(rng: RNG): (A, RNG) = State.run(self)(rng) // necessary for MonadSuite

  def apply[A](s: State[RNG, A]): Gen[A] = s

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  val boolean: Gen[Boolean] = State(RNG.boolean)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  val double: Gen[Double] = Gen(State(RNG.double))
  val int: Gen[Int] = Gen(State(RNG.int))

  def choose(i: Double, j: Double): Gen[Double] =
    State(RNG.double).map(d => i + d * (j - i))

  /* Basic idea is to add 1 to the result of `choose` if it is of the wrong
   * parity, but we require some special handling to deal with the maximum
   * integer in the range.
   */
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive % 2 == 0 then stopExclusive - 1 else stopExclusive).map(n =>
      if n % 2 != 0 then n + 1 else n
    )

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive % 2 != 0 then stopExclusive - 1 else stopExclusive).map(n =>
      if n % 2 == 0 then n + 1 else n
    )

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
    for
      i <- choose(from, to)
      j <- if i % 2 == 0 then even(from, to) else odd(from, to)
    yield (i, j)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  object ** :
    def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g.map(i => s => i)

  def genStringFn[A](g: Gen[A]): Gen[String => A] =
    State[RNG, String => A] { rng =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
      val f = (s: String) => g.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }

end Gen

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  extension [A](self: SGen[A])

    def apply(n: Int): Gen[A] = self(n)

    def map[B](f: A => B): SGen[B] =
      self(_).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(f(_)(n))

    def **[B](s2: SGen[B]): SGen[(A, B)] =
      n => Gen.**(apply(n))(s2(n))

  def apply[A](f: Int => Gen[A]): SGen[A] = f

opaque type Cogen[-A] = (A, RNG) => RNG

object Cogen:
  def fn[A, B](in: Cogen[A], out: Gen[B]): Gen[A => B] =
    State[RNG, A => B] { rng =>
      val (seed, rng2) = rng.nextInt
      val f = (a: A) => out.run(in(a, rng2))._1
      (f, rng2)
    }

  def cogenInt: Cogen[Int] = (i, rng) =>
    val (seed, rng2) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)
