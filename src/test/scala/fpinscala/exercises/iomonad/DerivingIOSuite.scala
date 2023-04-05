package fpinscala.exercises.iomonad

import fpinscala.answers.testing.exhaustive.Gen.int as genInt
import fpinscala.exercises.common.PropSuite
import IO3.Free.freeMonad
import IO3.Free.*
import IO3.Free

class DerivingIOSuite extends PropSuite:
  private val f: Int => Free[Function0, Int] = i => Free.Return(i)

//  private val g: Int => Free[Function0, Int] =
//    List.fill(10000)(f).foldLeft(f){ (a, b) => x =>
//      Free.Suspend(a(x).flatMap[Int](b))
//    }

  test("Exercise 13.1")(genInt) { n =>
    assertEquals(
      freeMonad.unit[Int](n),
      Free.Return(n)
    )

    assertEquals(
      freeMonad.unit[Int](n).flatMap[Int](f).step,
      Free.Return(n)
    )
  }

  test("Exercise 13.2")(genInt) { n =>
    assertEquals(
      f(n).runTrampoline,
      n
    )
  }
