package net.liftweb.json.macros

import scala.util.parsing.input.Position

/**
 * @author IL
 */
object JsParser extends Parser {
  def parseRaw(input: String)(p: (String, Position) => JsValue) = {
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(x, _) =>
        x
      case NoSuccess(msg, in) =>
        p(msg, in.pos)
    }
  }
}
