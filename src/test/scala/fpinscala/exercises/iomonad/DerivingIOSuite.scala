package fpinscala.exercises.iomonad

import fpinscala.answers.testing.exhaustive.Gen.int as genInt
import fpinscala.exercises.common.PropSuite
import IO3.Free.freeMonad
import IO3.Free.*
import IO3.Free

class DerivingIOSuite extends PropSuite:
  private val f: Int => Free[List, String] = i => Free.Return(i.toString)

  test("genMonad")(genInt) { n =>
    assertEquals(
      freeMonad.unit[Int](n),
      Free.Return(n)
    )

    assertEquals(
      freeMonad.unit[Int](n).flatMap[String](f).step,
      Free.Return(n.toString)
    )
  }

