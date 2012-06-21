package net.liftweb.json.macros

/**
 * @author IL
 */
object JsParser extends Parser {
  def parseRaw(input: String): JsValue = {
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => result
      case NoSuccess(message, _) => throw new Exception(message) // addexp
    }
  }
}
