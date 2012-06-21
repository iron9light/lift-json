package net.liftweb.json.macros

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.combinator.ImplicitConversions

/**
 * @author IL
 */
class Parser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  /**Type signature for functions that can parse numeric literals */
  type NumericParser = String => JsValue

  // Global default number parsing function
  protected var defaultNumberParser: NumericParser = {
    x => try {
      JsInt(BigInt(x))
    } catch {
      case e: NumberFormatException =>
        JsDouble(x.toDouble)
    }
  }

  // Per-thread default number parsing function
  protected val numberParser = new ThreadLocal[NumericParser]() {
    override def initialValue() = defaultNumberParser
  }

  // Define the grammar
  def root       = value
  def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[(JsFieldName, JsValue)] => JsObject(vals) }
  def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[JsValue] => JsArray(vals) }
  def objEntry   = (stringVal | identifier) ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[JsValue] = (jsonObj | jsonArray | number | "true" ^^^ JsTrue | "false" ^^^ JsFalse | "null" ^^^ JsNull | stringVal | identifier)
  def identifier = accept("identifier", { case lexical.Identifier(n) => JsId(n) })
  def stringVal  = accept("string", { case lexical.StringLit(n) => JsString(n) })
  def number = accept("number", {case lexical.NumericLit(n) => numberParser.get.apply(n) })
}

sealed abstract class JsValue
sealed trait JsFieldName

case object JsNull extends JsValue
case class JsString(s: String) extends JsValue with JsFieldName
case class JsDouble(num: Double) extends JsValue
case class JsInt(num: BigInt) extends JsValue
case object JsTrue extends JsValue
case object JsFalse extends JsValue
case class JsObject(obj: List[(JsFieldName, JsValue)]) extends JsValue
case class JsArray(arr: List[JsValue]) extends JsValue
case class JsId(id: String) extends JsValue with JsFieldName