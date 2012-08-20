import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

@RunWith(classOf[JUnitRunner])
class ScandSpec extends FunSpec {
  describe("Scand") {
    lazy val fooParser = Scand.stringParser("foo")
    lazy val barParser = Scand.stringParser("bar")
    lazy val bazParser = Scand.stringParser("baz")
    lazy val foo = new CharSequenceReader("foo")
    lazy val foofoo = new CharSequenceReader("foo foo")
    lazy val foofoobar = new CharSequenceReader("foo foo bar")
    lazy val foobar = new CharSequenceReader("foo bar")
    lazy val foobarbaz = new CharSequenceReader("foo bar baz")
    lazy val barfoo = new CharSequenceReader("bar foo")
    lazy val barfoobar = new CharSequenceReader("bar foo bar")
    lazy val barbazfoobazbar = new CharSequenceReader("bar baz foo baz bar")
    lazy val concat = (first: String, second: String) => first + second
    describe("andWithRepetition") {
      it("should parser a single parser normally") {
        val parser = Scand.andWithRepetition(List(fooParser), concat, "")
        parser(foo).get === "foo"
      }
      it("should parser two parsers forwards") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser), concat, "")
        parser(foobar).get === "foobar"
      }
      it("should parser two parsers backwards") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser), concat, "")
        parser(barfoo).get === "barfoo"
      }
      it("should parser three parsers forwards") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser, bazParser), concat, "")
        parser(foobarbaz).get === "foobarbaz"
      }
      it("should parser three parsers in every configuration") {
        for (list <- Util.reorder(List(fooParser, barParser, bazParser))) {
          val parser = Scand.andWithRepetition(list, concat, "")
          parser(foobarbaz).get === "foobarbaz"
        }
      }
      it("should handle repetition with only one arg.") {
        val parser = Scand.andWithRepetition(List(fooParser), concat, "")
        parser(foofoo).get === "foofoo"
      }
      it("should handle repetition with two args, in order.") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser), concat, "")
        parser(foofoobar).get === "foofoobar"
      }
      it("should handle repetition with two args, out of order.") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser), concat, "")
        parser(barfoobar).get === "barfoobar"
      }
      it("should handle repetition with three args, out of order.") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser, bazParser), concat, "")
        parser(barbazfoobazbar).get === "barbazfoobazbar"
      }
    }
    describe("andWithoutRepetition") {
      it("should parser a single parser normally") {
        val parser = Scand.andWithoutRepetition(List(fooParser), concat, "")
        parser(foo).get === "foo"
      }
      it("should parser two parsers forwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser), concat, "")
        parser(foobar).get === "foobar"
      }
      it("should parser two parsers backwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser), concat, "")
        parser(barfoo).get === "barfoo"
      }
      it("should parser three parsers forwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser, bazParser), concat, "")
        parser(foobarbaz).get === "foobarbaz"
      }
      it("should parser three parsers in every configuration") {
        for (list <- Util.reorder(List(fooParser, barParser, bazParser))) {
          val parser = Scand.andWithoutRepetition(list, concat, "")
          parser(foobarbaz).get === "foobarbaz"
        }
      }
    }
  }
  describe("Util") {
    describe("reorder") {
      val oneList = List(0)
      val twoList = List(0, 1)
      val threeList = List(0, 1, 2)
      it("Should reorder a 1-list") {
        Util.reorder(oneList) === List(oneList)
      }
      it("Should reorder a 2-list") {
        Util.reorder(twoList) === List(oneList, oneList.reverse)
      }
      it("Should reorder a 3-list") {
        Util.reorder(threeList) === List(threeList, threeList.reverse, List(1, 0, 2),
            List(2, 0, 1), List(1, 2, 0), List(0, 2, 1))
      }
    }
  }
}

object Util {
  def reorder[T](list: List[T]): List[List[T]] = list match {
    case Nil => List()
    case nonempty => nonempty map {elt =>
      reorder(list filterNot (_ == elt)) flatMap {
        elt :: _
      }
    }
  }
}