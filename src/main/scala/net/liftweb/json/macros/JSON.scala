package net.liftweb.json
package macros

import scala.reflect.makro.Context
import scala.language.experimental.macros

/**
 * @author IL
 */
object JSON {
  def apply(jsonSource: String): JValue = macro applyImpl
  
  def applyImpl(c: Context)(jsonSource: c.Expr[String]): c.Expr[JValue] = {
    import c.universe._

    object macroHelper extends MacroHelper {
      val context: c.type = c
    }

    import macroHelper._

    def jvalue2tree(j: JValue): c.Tree = {
      j match {
        case JObject(Nil) =>
          c.reify(JObject()).tree
        case JObject(obj) =>
          val xs = obj.map {
            case (k, v) =>
              pair(Literal(Constant(k)), jvalue2tree(v))
          }
          jObject(xs: _*)
        case JArray(Nil) =>
          c.reify(JArray(Nil)).tree
        case JArray(arr) =>
          val xs = arr.map(x => jvalue2tree(x))
          jArray(xs: _*)
        case JNull =>
          c.reify(JNull).tree
        case JString(s) =>
          jString(s)
        case JDouble(n) =>
          jDouble(n)
        case JInt(n) =>
          jInt(n)
        case JBool(b) =>
          jBool(b)
        case JNothing =>
          c.reify(JNothing).tree
      }
    }

    val Literal(Constant(s_jsonSource: String)) = jsonSource.tree
    val jValue = parse(s_jsonSource)

    c.Expr[JValue](jvalue2tree(jValue))
  }
}