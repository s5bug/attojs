package tf.bug.attojs

sealed trait JsStat

sealed trait JsExpr extends JsStat

sealed trait JsLiteral extends JsExpr

final case object JsNull extends JsLiteral

sealed trait JsBoolean extends JsLiteral
final case object JsFalse extends JsBoolean
final case object JsTrue extends JsBoolean

sealed trait JsNumber extends JsLiteral
final case class JsDecimal(value: Double) extends JsNumber
final case class JsHex(value: Double) extends JsNumber
final case class JsOctal(value: Double) extends JsNumber
final case class JsBinary(value: Double) extends JsNumber

sealed trait JsKeyword extends JsStat

final case class JsReturn(value: Option[JsExpr]) extends JsKeyword

sealed trait JsComment extends JsStat
final case class JsSingleLineComment(text: String) extends JsComment
final case class JsMultiLineComment(text: String) extends JsComment
