package tf.bug.attojs

import atto._
import atto.Atto._
import cats._
import cats.implicits._

object JsParser {

  val jsLineTerminator: Parser[Unit] = {
    string("\n") | string("\r") | string("\u2028") | string("\u2029")
  }.void

  val jsLineTerminatorSequence: Parser[Unit] = {
    string("\n") | string("\r\n") | string("\r") | string("\u2028") | string("\u2029")
  }.void

  lazy val jsLiteralNull: Parser[JsNull.type] = string("null") >| JsNull

  lazy val jsBooleanFalse: Parser[JsFalse.type] = string("false") >| JsFalse
  lazy val jsBooleanTrue: Parser[JsTrue.type] = string("true") >| JsTrue
  val jsLiteralBoolean: Parser[JsBoolean] = jsBooleanFalse | jsBooleanTrue

  lazy val jsNumberDecimal: Parser[JsDecimal] = double.map(JsDecimal)
  lazy val jsNumberHex: Parser[JsHex] =
    string("0x") ~>
      many1(hexDigit)
        .map(l => "0x" ++ new String(l.toList.toArray) ++ "p0")
        .map(java.lang.Double.valueOf)
        .map(JsHex(_))
  lazy val jsNumberOctal: Parser[JsOctal] = (string("0") | string("0o")) ~> err("not implemented")
  lazy val jsNumberBinary: Parser[JsBinary] = string("0b") ~> err("not implemented")
  val jsLiteralNumber: Parser[JsNumber] = jsNumberOctal | jsNumberBinary | jsNumberHex | jsNumberDecimal

  val jsLiteral: Parser[JsLiteral] = jsLiteralNull | jsLiteralBoolean | jsLiteralNumber

  lazy val jsKeywordReturn: Parser[JsReturn] =
    (string("return ") ~> jsExpr).map(s => JsReturn(s.some)) | (string("return") >| JsReturn(None))

  val jsKeyword: Parser[JsKeyword] = jsKeywordReturn

  // FIXME needs to use lookahead and properly handle multiline comments
  lazy val jsSingleLineComment: Parser[JsSingleLineComment] =
    (string("//") ~> manyUntil(anyChar, jsLineTerminator).map(l => new String(l.toArray))).map(JsSingleLineComment)
  lazy val jsMultiLineComment: Parser[JsMultiLineComment] =
    bracket(string("/*"), many(anyChar), string("*/")).map(l => new String(l.toArray)).map(JsMultiLineComment)
  val jsComment: Parser[JsComment] = jsSingleLineComment | jsMultiLineComment

  val jsExpr: Parser[JsExpr] = jsLiteral

  val jsStat: Parser[JsStat] = jsExpr | jsKeyword | jsComment

}
