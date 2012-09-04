import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object Scand extends RegexParsers {
  def andWithRepetition[T](parsed: List[Parser[T]],
    unparsed: List[Parser[T]],
    reducer: (T, T) => T,
    default: T,
    disabler: Parser[T] => Parser[T]): Parser[T] =
      andWithRepetition(ParserHelper(parsed, unparsed, reducer, default, disabler))

  def andWithoutRepetition[T](list: List[Parser[T]],
    reducer: (T, T) => T,
    default: T,
    disabler: Parser[T] => Parser[T]): Parser[T] =
      andWithoutRepetition(ParserHelper(List(), list, reducer, default, disabler))

  def andWithoutRepetition[T](helper: ParserHelper[T]): Parser[T] =
      parseList(helper) getOrElse success(helper.default)

  private[this] def andWithRepetition[T](helper: ParserHelper[T]): Parser[T] = Pair(helper.unparsed, helper.parsed) match {
    case (Nil, Nil) => success(helper.default)
    case (Nil, list) => rep(list reduce (_ | _)) map {arg =>
      arg reduceOption (helper.reducer(_, _)) getOrElse helper.default
    }
    case (_, Nil) => parseUnparsed(helper)
    case (_, _) => parseUnparsed(helper) | parseParsed(helper)
  }

  private[this] def parseParsed[T](helper: ParserHelper[T]) =
    (helper.parsed reduce (_ | _)) ~ andWithRepetition(helper) map {
      applyReducer(_, helper.reducer)
    }

  def dontbreak: Parser[String] =
    rep(List(discardEmpties(opt("string") map {_ getOrElse ("")})) reduce (_ | _)) map {list =>
      list reduce (_ + _)
    }

  def break: Parser[String] =
    rep(List(opt("string") map {_ getOrElse ("")}) reduce (_ | _)) map {list =>
      list reduce (_ + _)
    }

  private[this] def parseUnparsed[T](helper: ParserHelper[T]) = (helper.unparsed map {parser =>
    parser ~ andWithRepetition(helper.useParser(parser)) map (
      applyReducer(_, helper.reducer)
    )
  }) reduce (_ | _)

  private[this] def parseList[T](helper: ParserHelper[T]) = (helper.unparsed map
      (andThenParse(_, helper))
    ) reduceOption (_ | _)

  private[this] def reduceOrWithDefault[U](list: List[Parser[U]], default: Parser[U]): Parser[U] =
    list reduceOption (_ | _) getOrElse default

  def stringParser(string: String): Parser[String] = string

  def optStringParser(string: String): Parser[String] = opt(string) map (_ getOrElse "")

  private[this] def andThenParse[T](parser: Parser[T],
    helper: ParserHelper[T]): Parser[T] = {
    parser ~ andWithoutRepetition(helper.useParser(parser)) map (
      applyReducer(_, helper.reducer)
    )
  }

  private[this] def applyReducer[T](concat: ~[T, T], reducer: (T, T) => T) = concat match {
    case f ~ s => reducer(f, s)
  }

  def discardEmpties(parser: Parser[String]): Parser[String] = Parser { input =>
    parser(input) match {
      case Success("", _) => Failure("No empties", input)
      case s @ Success(_, _) => s
      case noSuccess => noSuccess
    }
  }

  case class ParserHelper[T](parsed: List[Parser[T]],
    unparsed: List[Parser[T]],
    reducer: (T, T) => T,
    default: T,
    disabler: Parser[T] => Parser[T]) {
    def useParser(parser: Parser[T]): ParserHelper[T] = this.copy(parsed = disabler(parser) :: this.parsed,
      unparsed = this.unparsed filterNot (_ == parser))
  }

  object ParserHelper {
    def apply[T](list: List[Parser[T]],
        reducer: (T, T) => T,
        default: T,
        disabler: Parser[T] => Parser[T]): ParserHelper[T] =
      ParserHelper(List(), list, reducer, default, disabler)
  }
}
