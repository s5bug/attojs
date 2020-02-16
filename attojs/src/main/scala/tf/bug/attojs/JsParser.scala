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

  lazy val jsIdentifier: Parser[JsIdentifier] = {
    val identifierStart = elem(c => c.isLetter || c === '$' || c == '_')
    val identifierPart = identifierStart | elem { c =>
      c.isDigit ||
        Character.getType(c) === Character.CONNECTOR_PUNCTUATION ||
        Character.getType(c) === Character.COMBINING_SPACING_MARK ||
        c === '\u200d' ||
        c === '\u200c'
    }
    (identifierStart ~ many(identifierPart)).map {
      case (h, t) => new String((h :: t).toArray)
    }.map(JsIdentifier)
  }

  lazy val jsProperty: Parser[JsProperty] = (jsExpr ~ (string(".") ~> jsIdentifier)).map(JsProperty.tupled)

  lazy val jsKeywordReturn: Parser[JsReturn] =
    (string("return ") ~> jsExpr).map(s => JsReturn(s.some)) | (string("return") >| JsReturn(None))

  lazy val parseArguments: Parser[Vector[JsIdentifier]] =
    parens(sepBy(jsIdentifier, skipWhitespace ~> string(",") <~ skipWhitespace)).map(_.toVector)

  lazy val jsKeywordNamedFunction: Parser[JsNamedFunction] =
    (string("function ") ~> jsIdentifier ~ parseArguments ~ (skipWhitespace ~> braces(many(jsStat)))).map {
      case ((name, arguments), content) =>
        JsNamedFunction(name, arguments, content.toVector)
    }
  lazy val jsKeywordAnonymousFunction: Parser[JsAnonymousFunction] =
    (string("function") ~> parseArguments ~ (skipWhitespace ~> braces(many(jsStat)))).map {
      case (arguments, content) =>
        JsAnonymousFunction(arguments, content.toVector)
    }

  lazy val jsKeywordFor: Parser[JsFor] = {
    val head = string("for") ~> parens(delay(jsStat) ~ delay(jsExpr) ~ delay(jsStat))
    val singleBody = delay(jsStat).map(_.asLeft)
    val multiBody = braces(many(delay(jsStat))).map(_.toVector.asRight)
    val uncomposed: Parser[(((JsStat, JsExpr), JsStat), Either[JsStat, Vector[JsStat]])] = head ~ (singleBody | multiBody)
    uncomposed.map {
      case (((initial, condition), every), body) => JsFor(initial, condition, every, body)
    }
  }

  lazy val jsApply: Parser[JsApply] =
    (jsExpr ~ parens(sepBy(jsExpr, skipWhitespace ~> string(",") <~ skipWhitespace))).map {
      case (function, args) => JsApply(function, args.toVector)
    }

  val jsKeyword: Parser[JsStat] = jsKeywordReturn | jsKeywordNamedFunction | jsKeywordFor

  // FIXME needs to use lookahead and properly handle multiline comments
  lazy val jsSingleLineComment: Parser[JsSingleLineComment] =
    (string("//") ~> manyUntil(anyChar, jsLineTerminator).map(l => new String(l.toArray))).map(JsSingleLineComment)
  lazy val jsMultiLineComment: Parser[JsMultiLineComment] =
    (string("/*") ~> many(notChar('*') | (char('*') ~> notChar('/'))) <~ string("*/")).map(l => new String(l.toArray)).map(JsMultiLineComment)
  val jsComment: Parser[JsComment] = jsSingleLineComment | jsMultiLineComment

  val jsTerm: Parser[JsTerm] = jsKeywordNamedFunction | jsLiteral | jsIdentifier

  val jsExpr: Parser[JsExpr] = jsKeywordAnonymousFunction | jsTerm

  val jsStat: Parser[JsStat] = jsKeyword | jsTerm | jsComment

  val js: Parser[Js] = jsStat | jsExpr

}
