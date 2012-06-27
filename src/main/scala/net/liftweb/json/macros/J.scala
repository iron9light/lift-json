package net.liftweb.json
package macros

import scala.reflect.makro.Context
import scala.language.experimental.macros
import scala.util.parsing.input.{Position => SPosition, OffsetPosition}

/**
 * @author IL
 */
object J {
  def apply(jsSource: String): JValue = macro applyImpl

  def applyImpl(c: Context)(jsSource: c.Expr[String]): c.Expr[JValue] = {
    import c.universe._

    val s_jsSource = jsSource.tree match {
      case Literal(Constant(s: String)) =>
        s
      case _ =>
        c.info(c.enclosingPosition, showRaw(jsSource.tree), false)
        c.abort(c.enclosingPosition, "jsSource must be a string literal.")
    }
    
    implicit val offset2pos = {
      val pos = jsSource.tree.pos
      val point = pos.point
      val content = pos.fileContent
      val fixedPoint = if (content.length >= point + 2 && content(point) == '\"' && content(point + 1) == '\"' && content(point + 2) == '\"') {
        point + 3
      } else {
        point + 1
      }
      
      (position: SPosition) => position match {
        case OffsetPosition(_, offset) =>
          pos.withPoint(fixedPoint + offset)
        case _ =>
          c.enclosingPosition
          
      }
    }

    object macroHelper extends MacroHelper {
      val context: c.type = c
    }

    import macroHelper._

    def js2tree(j: JsValue): c.Tree = {
      val tree = j match {
        case JsNull =>
          c.reify(JNull).tree
        case JsString(s) =>
          jString(s)
        case JsDouble(num) =>
          jDouble(num)
        case JsInt(num) =>
          jInt(num)
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
                  variable[String](id)
              }
              pair(fieldName, js2tree(v))
          }
          jObject(xs: _*)
        case JsArray(Nil) =>
          c.reify(JArray(Nil)).tree
        case JsArray(arr) =>
          val xs = arr.map(x => js2tree(x))
          jArray(xs: _*)
        case JsId(id) =>
          variable[JValue](id)
      }
      
      tree.setPos(j.pos)
    }

    val jsValue = JsParser.parseRaw(s_jsSource){
      case (msg, pos) =>
        c.abort(pos, msg)
    }

    c.Expr[JValue](js2tree(jsValue))
  }
}