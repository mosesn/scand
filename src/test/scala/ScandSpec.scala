import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ScandSpec extends FunSpec with ShouldMatchers {
  describe("Scand") {
    lazy val fooParser = Scand.stringParser("foo")
    lazy val optFooParser = Scand.optStringParser("foo")
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
        val parser = Scand.andWithRepetition(List(), List(fooParser), concat, "", Scand.discardEmpties)
        assert(parser(foo).get === "foo")
      }
      it("should parser two parsers forwards") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(foobar).get === "foobar")
      }
      it("should parser two parsers backwards") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(barfoo).get === "barfoo")
      }
      it("should parser three parsers forwards") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser, bazParser), concat, "", Scand.discardEmpties)
        assert(parser(foobarbaz).get === "foobarbaz")
      }
      it("should parser three parsers in every configuration") {
        for (list <- Util.reorder(List(fooParser, barParser, bazParser))) {
          val parser = Scand.andWithRepetition(List(), list, concat, "", Scand.discardEmpties)
          assert(parser(foobarbaz).get === "foobarbaz")
        }
      }
      it("should handle repetition with only one arg.") {
        val parser = Scand.andWithRepetition(List(), List(fooParser), concat, "", Scand.discardEmpties)
        assert(parser(foofoo).get === "foofoo")
      }
      it("should handle repetition with two args, in order.") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(foofoobar).get === "foofoobar")
      }
      it("should handle repetition with two args, out of order.") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(barfoobar).get === "barfoobar")
      }
      it("should handle repetition with three args, out of order.") {
        val parser = Scand.andWithRepetition(List(), List(fooParser, barParser, bazParser), concat, "", Scand.discardEmpties)
        assert(parser(barbazfoobazbar).get === "barbazfoobazbar")
      }
      it("should parser a single optional parser normally") {
        val parser = Scand.andWithRepetition(List(fooParser), List(), concat, "", Scand.discardEmpties)
        assert(parser(foo).get === "foo")
      }
      it("should parser two optional parsers forwards") {
        val parser = Scand.andWithRepetition(List(fooParser, barParser), List(), concat, "", Scand.discardEmpties)
        assert(parser(foobar).get === "foobar")
      }
      it("should parser a properly optional parser forwards") {
        val parser = Scand.andWithRepetition(Nil, List(optFooParser), concat, "", Scand.discardEmpties)
        assert(parser(foo).get === "foo")
      }
    }
    describe("andWithoutRepetition") {
      it("should parser a single parser normally") {
        val parser = Scand.andWithoutRepetition(List(fooParser), concat, "", Scand.discardEmpties)
        assert(parser(foo).get === "foo")
      }
      it("should parser two parsers forwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(foobar).get === "foobar")
      }
      it("should parser two parsers backwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser), concat, "", Scand.discardEmpties)
        assert(parser(barfoo).get === "barfoo")
      }
      it("should parser three parsers forwards") {
        val parser = Scand.andWithoutRepetition(List(fooParser, barParser, bazParser), concat, "", Scand.discardEmpties)
        assert(parser(foobarbaz).get === "foobarbaz")
      }
      it("should parser three parsers in every configuration") {
        for (list <- Util.reorder(List(fooParser, barParser, bazParser))) {
          val parser = Scand.andWithoutRepetition(list, concat, "", Scand.discardEmpties)
          assert(parser(foobarbaz).get === ("foobarbaz"))
        }
      }
    }


    describe("discardEmpties") {
      it("should discard empties properly") {
        assert(Scand.discardEmpties(optFooParser)(foo).get === "foo")
      }
    }
  }
  describe("Util") {
    describe("reorder") {
      val oneList = List(0)
      val twoList = List(0, 1)
      val threeList = List(0, 1, 2)
      it("Should reorder a 1-list") {
        assert(Util.reorder(oneList).toSet === Set(oneList))
      }
      it("Should reorder a 2-list") {
        assert(Util.reorder(twoList).toSet === Set(twoList, twoList.reverse))
      }
      it("Should reorder a 3-list") {
        assert(Util.reorder(threeList).toSet === Set(threeList, threeList.reverse, List(1, 0, 2),
            List(2, 0, 1), List(1, 2, 0), List(0, 2, 1)))
      }
    }
  }

}

object Util {
  def reorder[T](list: List[T]): List[List[T]] = list match {
    case Nil => List(List())
    case nonempty => nonempty flatMap {elt =>
      reorder(list filterNot (_ == elt)) map (elt :: _)
    }
  }
}
