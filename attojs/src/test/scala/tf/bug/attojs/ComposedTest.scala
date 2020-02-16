package tf.bug.attojs

import atto._
import atto.Atto._
import cats._
import cats.implicits._
import org.scalatest.wordspec.AnyWordSpec

class ComposedTest extends AnyWordSpec {

  val fizzBuzz: String =
    """for (var i=1; i <= 20; i++) {
      |    if (i % 15 == 0)
      |        console.log("FizzBuzz");
      |    else if (i % 3 == 0)
      |        console.log("Fizz");
      |    else if (i % 5 == 0)
      |        console.log("Buzz");
      |    else
      |        console.log(i);
      |}
      |""".stripMargin

  "js" when {
    "fed fizzBuzz" should {
      "output the proper AST" in {
        assertResult(
          ParseResult.Done(
            "",
            JsFor(
              JsVar(
                JsIdentifier("i"),
                Some(JsDecimal(1.0d))
              ),
              JsLessThanOrEqualTo(
                JsIdentifier("i"),
                JsDecimal(20.0d)
              ),
              JsIncrement(
                JsIdentifier("i")
              ),
              Right(
                Vector(
                  JsIf(
                    JsIfCase(
                      JsEqualTo(
                        JsModulo(
                          JsIdentifier("i"),
                          JsDecimal(15.0d)
                        ),
                        JsDecimal(0.0d)
                      ),
                      Left(
                        JsApply(
                          JsProperty(
                            JsIdentifier("console"),
                            JsIdentifier("log")
                          ),
                          Vector(
                            JsString("FizzBuzz")
                          )
                        )
                      )
                    ),
                    Vector(
                      JsIfCase(
                        JsEqualTo(
                          JsModulo(
                            JsIdentifier("i"),
                            JsDecimal(3.0d)
                          ),
                          JsDecimal(0.0d)
                        ),
                        Left(
                          JsApply(
                            JsProperty(
                              JsIdentifier("console"),
                              JsIdentifier("log")
                            ),
                            Vector(
                              JsString("Fizz")
                            )
                          )
                        )
                      ),
                      JsIfCase(
                        JsEqualTo(
                          JsModulo(
                            JsIdentifier("i"),
                            JsDecimal(5.0d)
                          ),
                          JsDecimal(0.0d)
                        ),
                        Left(
                          JsApply(
                            JsProperty(
                              JsIdentifier("console"),
                              JsIdentifier("log")
                            ),
                            Vector(
                              JsString("Buzz")
                            )
                          )
                        )
                      )
                    ),
                    Some(
                      JsApply(
                        JsProperty(
                          JsIdentifier("console"),
                          JsIdentifier("log")
                        ),
                        Vector(
                          JsIdentifier("i")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )(
          JsParser.js.parseOnly(fizzBuzz)
        )
      }
    }
  }

}
