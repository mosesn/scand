import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

object Scand extends RegexParsers {
  def and[T](list: List[Parser[T]],
      reducer: (T, T) => T,
      default: T): Parser[T] = reduceOrWithDefault(list map {parser =>
        parser ~ and(list filterNot (_ == parser), reducer, default) map {
          case f ~ s => reducer(f, s)
        }
      }, success(default))

  def reduceOrWithDefault[U](list: List[Parser[U]], default: Parser[U]): Parser[U] =
    list reduceOption (_ | _) getOrElse default

  def stringParser(string: String): Parser[String] = string
}