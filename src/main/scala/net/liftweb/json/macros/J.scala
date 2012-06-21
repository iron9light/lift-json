package net.liftweb.json
package macros

import scala.reflect.makro.Context
import scala.language.experimental.macros

/**
 * @author IL
 */
object J {
  def apply(jsSource: String): JValue = macro applyImpl

  def applyImpl(c: Context)(jsSource: c.Expr[String]): c.Expr[JValue] = {
    import c.universe._
    import MacroHelper._

    val Literal(Constant(s_jsSource: String)) = jsSource.tree

    def js2tree(j: JsValue): c.Tree = {
      j match {
        case JsNull =>
          c.reify(JNull).tree
        case JsString(s) =>
          jString(c)(s)
        case JsDouble(num) =>
          jDouble(c)(num)
        case JsInt(num) =>
          jInt(c)(num)
        case JsTrue =>
          c.reify(JBool(true)).tree
        case JsFalse =>
          c.reify(JBool(false)).tree
        case JsObject(Nil) =>
          c.reify(JObject()).tree
        case JsObject(obj) =>
          val xs = obj map {
            case (k, v) =>
              val fieldName = k match {
                case JsString(s) =>
                  c.literal(s).tree
                case JsId(id) =>
                  variable[String](c)(id)
              }
              pair(c)(fieldName, js2tree(v))
          }
          jObject(c)(xs: _*)
        case JsArray(Nil) =>
          c.reify(JArray(Nil)).tree
        case JsArray(arr) =>
          val xs = arr.map(x => js2tree(x))
          jArray(c)(xs: _*)
        case JsId(id) =>
          variable[JValue](c)(id)
      }
    }

    val jsValue = JsParser.parseRaw(s_jsSource)

    c.Expr[JValue](js2tree(jsValue))
  }
}