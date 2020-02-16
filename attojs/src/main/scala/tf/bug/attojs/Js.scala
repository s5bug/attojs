package tf.bug.attojs

sealed trait Js {

}

sealed trait JsStat extends Js

sealed trait JsExpr extends Js

sealed trait JsTerm extends JsExpr with JsStat

sealed trait JsLiteral extends JsTerm

final case object JsNull extends JsLiteral

sealed trait JsBoolean extends JsLiteral
final case object JsFalse extends JsBoolean
final case object JsTrue extends JsBoolean

sealed trait JsNumber extends JsLiteral
final case class JsDecimal(value: Double) extends JsNumber
final case class JsHex(value: Double) extends JsNumber
final case class JsOctal(value: Double) extends JsNumber
final case class JsBinary(value: Double) extends JsNumber

final case class JsString(value: String) extends JsLiteral

final case class JsIdentifier(name: String) extends JsTerm
final case class JsProperty(base: JsExpr, property: JsIdentifier) extends JsTerm

final case class JsReturn(value: Option[JsExpr]) extends JsStat

final case class JsVar(name: JsIdentifier, initializeTo: Option[JsExpr]) extends JsStat

sealed trait JsFunction extends JsExpr
final case class JsNamedFunction(name: JsIdentifier, arguments: Vector[JsIdentifier], content: Vector[JsStat]) extends JsFunction with JsTerm
final case class JsAnonymousFunction(arguments: Vector[JsIdentifier], content: Vector[JsStat]) extends JsFunction

final case class JsIfCase(condition: JsExpr, body: Either[JsStat, Vector[JsStat]])
final case class JsIf(head: JsIfCase, elseIfs: Vector[JsIfCase], elseCase: Option[JsStat]) extends JsStat

final case class JsFor(initial: JsStat, condition: JsExpr, every: JsStat, body: Either[JsStat, Vector[JsStat]]) extends JsStat

final case class JsApply(function: JsExpr, arguments: Vector[JsExpr]) extends JsTerm

final case class JsEqualTo(lhs: JsExpr, rhs: JsExpr) extends JsTerm
final case class JsLessThanOrEqualTo(lhs: JsExpr, rhs: JsExpr) extends JsTerm

final case class JsIncrement(value: JsIdentifier) extends JsTerm

final case class JsModulo(lhs: JsExpr, rhs: JsExpr) extends JsTerm

sealed trait JsComment extends JsStat
final case class JsSingleLineComment(text: String) extends JsComment
final case class JsMultiLineComment(text: String) extends JsComment
