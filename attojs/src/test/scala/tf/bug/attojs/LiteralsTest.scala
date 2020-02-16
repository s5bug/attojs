package tf.bug.attojs

import atto._
import atto.Atto._
import cats._
import cats.implicits._
import org.scalatest.wordspec.AnyWordSpec

class LiteralsTest extends AnyWordSpec {

  "jsLiteralNull" when {
    "fed \"null\"" should {
      "output JsNull" in {
        assertResult(ParseResult.Done("", JsNull))(JsParser.jsLiteralNull.parseOnly("null"))
      }
    }
    "fed \"notNull\"" should {
      "fail" in {
        assert(JsParser.jsLiteralNull.parseOnly("notNull") match {
          case ParseResult.Fail(_, _, _) => true
          case _ => false
        })
      }
    }
  }

  "jsLiteralBoolean" when {
    "fed \"false\"" should {
      "output JsFalse" in {
        assertResult(ParseResult.Done("", JsFalse))(JsParser.jsLiteralBoolean.parseOnly("false"))
      }
    }
    "fed \"true\"" should {
      "output JsTrue" in {
        assertResult(ParseResult.Done("", JsTrue))(JsParser.jsLiteralBoolean.parseOnly("true"))
      }
    }
    "fed \"notABoolean\"" should {
      "fail" in {
        assert(JsParser.jsLiteralBoolean.parseOnly("notABoolean") match {
          case ParseResult.Fail(_, _, _) => true
          case _ => false
        })
      }
    }
  }

  "jsLiteralNumber" when {
    "fed \"0\"" should {
      "output JsDecimal(0.0d)" in {
        assertResult(ParseResult.Done("", JsDecimal(0.0d)))(JsParser.jsLiteralNumber.parseOnly("0"))
      }
    }
    "fed \"1.5\"" should {
      "output JsDecimal(1.5d)" in {
        assertResult(ParseResult.Done("", JsDecimal(1.5d)))(JsParser.jsLiteralNumber.parseOnly("1.5"))
      }
    }
    "fed \"1.4e+5\"" should {
      "output JsDecimal(140000.0d)" in {
        assertResult(ParseResult.Done("", JsDecimal(140000.0d)))(JsParser.jsLiteralNumber.parseOnly("1.4e+5"))
      }
    }
    "fed \"0x7f\"" should {
      "output JsHex(127.0d)" in {
        assertResult(ParseResult.Done("", JsHex(127.0d)))(JsParser.jsLiteralNumber.parseOnly("0x7f"))
      }
    }
    "fed \"0o10\"" should {
      "output JsOctal(8.0d)" in {
        assertResult(ParseResult.Done("", JsOctal(8.0d)))(JsParser.jsLiteralNumber.parseOnly("0o10"))
      }
    }
    "fed \"010\"" should {
      "output JsOctal(8.0d)" in {
        assertResult(ParseResult.Done("", JsOctal(8.0d)))(JsParser.jsLiteralNumber.parseOnly("010"))
      }
    }
    "fed 0b10" should {
      "output JsBinary(2.0d)" in {
        assertResult(ParseResult.Done("", JsOctal(2.0d)))(JsParser.jsLiteralNumber.parseOnly("0b10"))
      }
    }
  }

}
