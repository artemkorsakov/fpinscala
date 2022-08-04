package fpinscala.exercises.parsing

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] = ???

  extension[A] (p: Parser[A])

    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]

    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]] = ???

    def map[B](f: A => B): Parser[B] = ???

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = ???

    def many: Parser[List[A]] = ???

    def many1: Parser[List[A]] = ???

    def product[B](p2: => Parser[B]): Parser[(A, B)] = ???

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0):

  lazy val line: Int = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int): String = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

end Location

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  lazy val nonNegativeInt: Parser[Int] = ???

  lazy val nConsecutiveAs: Parser[Int] = ???
