package tf.bug.attojs

import atto._
import atto.Atto._
import cats._
import cats.implicits._
import org.scalatest.wordspec.AnyWordSpec

class KeywordsTest extends AnyWordSpec {

  "jsKeywordReturn" when {
    "fed \"return\"" should {
      "output JsReturn(None)" in {
        assertResult(ParseResult.Done("", JsReturn(None)))(JsParser.jsKeywordReturn.parseOnly("return"))
      }
    }
    "fed \"return null\"" should {
      "output JsReturn(Some(JsNull))" in {
        assertResult(ParseResult.Done("", JsReturn(Some(JsNull))))(JsParser.jsKeywordReturn.parseOnly("return null"))
      }
    }
    "fed \"return 1.4e5\"" should {
      "output JsReturn(Some(JsDecimal(140000.0d)))" in {
        assertResult(ParseResult.Done("", JsReturn(Some(JsDecimal(140000.0d)))))(JsParser.jsKeywordReturn.parseOnly("return 1.4e5"))
      }
    }
  }

}
