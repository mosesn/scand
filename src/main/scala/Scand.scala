import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

object Scand extends RegexParsers {
  def andWithRepetition[T](list: List[Parser[T]],
    reducer: (T, T) => T,
    default: T): Parser[T] = andWithRepetition(ParserHelper(List(), list, reducer, default))

  def andWithoutRepetition[T](list: List[Parser[T]],
    reducer: (T, T) => T,
    default: T): Parser[T] =
      andWithoutRepetition(ParserHelper(List(), list, reducer, default))

  def andWithoutRepetition[T](helper: ParserHelper[T]): Parser[T] =
      parseList(helper) getOrElse success(helper.default)

  private[this] def andWithRepetition[T](helper: ParserHelper[T]): Parser[T] = {
    val unparsedProgress = parseUnparsed(helper)
    val parsedProgress = parseParsed(helper) getOrElse success(helper.default)
    unparsedProgress map (_ | parsedProgress) getOrElse parsedProgress
  }

  private[this] def parseParsed[T](helper: ParserHelper[T]) =
    (helper.parsed reduceOption (_ | _)) map (
      _ ~ andWithRepetition(helper) map {
        applyReducer(_, helper.reducer)
      }
    )

  private[this] def parseUnparsed[T](helper: ParserHelper[T]) = (helper.unparsed map {parser =>
    parser ~ andWithRepetition(helper.useParser(parser)) map (
      applyReducer(_, helper.reducer)
    )
  }) reduceOption (_ | _)

  private[this] def parseList[T](helper: ParserHelper[T]) = (helper.unparsed map
      (andThenParse(_, helper))
    ) reduceOption (_ | _)

  private[this] def reduceOrWithDefault[U](list: List[Parser[U]], default: Parser[U]): Parser[U] =
    list reduceOption (_ | _) getOrElse default

  def stringParser(string: String): Parser[String] = string

  private[this] def andThenParse[T](parser: Parser[T],
    helper: ParserHelper[T]): Parser[T] = {
    parser ~ andWithoutRepetition(helper.useParser(parser)) map (
      applyReducer(_, helper.reducer)
    )
  }

  private[this] def applyReducer[T](concat: ~[T, T], reducer: (T, T) => T) = concat match {
    case f ~ s => reducer(f, s)
  }

  case class ParserHelper[T](parsed: List[Parser[T]],
    unparsed: List[Parser[T]],
    reducer: (T, T) => T,
    default: T) {
    def useParser(parser: Parser[T]) = this.copy(parsed = parser :: this.parsed,
      unparsed = this.unparsed filterNot (_ == parser))
  }

  object ParserHelper {
    def apply[T](list: List[Parser[T]], reducer: (T, T) => T, default: T): ParserHelper[T] =
      ParserHelper(List(), list, reducer, default)
  }
}