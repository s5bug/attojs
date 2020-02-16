package tf.bug.attojs

import atto._
import atto.Atto._
import org.scalatest.wordspec.AnyWordSpec

class CommentsTest extends AnyWordSpec {

  "jsComment" when {
    "fed \"// test\\n\"" should {
      "output JsSingleLineComment( test)" in {
        assertResult(ParseResult.Done("\n", JsSingleLineComment(" test")))(JsParser.jsComment.parseOnly("// test\n"))
      }
    }
    "fed \"/* test */\"" should {
      "output JsMultiLineComment( test )" in {
        assertResult(ParseResult.Done("", JsMultiLineComment(" test ")))(JsParser.jsComment.parseOnly("/* test */"))
      }
    }
    "fed \"/*\\n  test\\n*/\"" should {
      "output JsMultiLineComment(\\n  test\\n)" in {
        assertResult(ParseResult.Done("", JsMultiLineComment("\n  test\n")))(JsParser.jsComment.parseOnly("/*\n  test\n*/"))
      }
    }
    "fed \"/*foo*/bar*/\"" should {
      "output JsMultiLineComment(foo)" in {
        assertResult(Some(JsMultiLineComment("foo")))(JsParser.jsComment.parseOnly("/*foo*/bar*/").option)
      }
      "leave \"bar*/\" remaining" in {
        assertResult(ParseResult.Done("bar*/", JsMultiLineComment("foo")))(JsParser.jsComment.parseOnly("/*foo*/bar*/"))
      }
    }
  }

}
