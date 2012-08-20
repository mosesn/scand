import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

object Scand extends RegexParsers {
  def andWithRepetition[T](list: List[Parser[T]],
      reducer: (T, T) => T,
      default: T): Parser[T] = andWithRepetition(List(), list, reducer, default)

  def andWithoutRepetition[T](list: List[Parser[T]],
      reducer: (T, T) => T,
      default: T): Parser[T] = parseList(list, reducer, default) getOrElse success(default)

  def andWithRepetition[T](parsed: List[Parser[T]],
      unparsed: List[Parser[T]],
      reducer: (T, T) => T,
      default: T): Parser[T] = {
        val unparsedProgress = parseUnparsed(parsed, unparsed, reducer, default)
        val parsedProgress = parseParsed(parsed, unparsed, reducer, default) getOrElse
          success(default)
        unparsedProgress map (_ | parsedProgress) getOrElse parsedProgress
      }

  def parseParsed[T](parsed: List[Parser[T]],
      unparsed: List[Parser[T]],
      reducer: (T, T) => T,
      default: T) = (parsed reduceOption (_ | _)) map (
        _ ~ andWithRepetition(parsed, unparsed, reducer, default) map {
          case f ~ s => reducer(f, s)
        }
      )

  def parseUnparsed[T](parsed: List[Parser[T]],
      unparsed: List[Parser[T]],
      reducer: (T, T) => T,
      default: T) = (unparsed map {parser =>
        parser ~ andWithRepetition(parser :: parsed, unparsed filterNot (_ == parser), reducer, default) map {
          case f ~ s => reducer(f, s)
        }
      }) reduceOption (_ | _)

  def parseList[T](unparsed: List[Parser[T]],
      reducer: (T, T) => T,
      default: T) = (unparsed map {parser =>
        parser ~ andWithoutRepetition(unparsed filterNot (_ == parser), reducer, default) map {
          case f ~ s => reducer(f, s)
        }
      }) reduceOption (_ | _)

  def reduceOrWithDefault[U](list: List[Parser[U]], default: Parser[U]): Parser[U] =
    list reduceOption (_ | _) getOrElse default

  def stringParser(string: String): Parser[String] = string
}