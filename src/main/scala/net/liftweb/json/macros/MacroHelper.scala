package net.liftweb.json
package macros

import scala.reflect.makro.Context

/**
 * @author IL
 */
private[macros] abstract class MacroHelper {
  val context: Context

  import context.literal
  import context.universe._

  def list(xs: Tree*) = {
    val Apply(fun, _) = reify(List(0)).tree
    Apply.apply(fun, xs.toList)
  }

  def jArray(xs: Tree*) = {
    val Apply(fun, _) = reify(JArray(Nil)).tree
    Apply.apply(fun, list(xs: _*) :: Nil)
  }

  def pair(_1: Tree, _2: Tree) = {
    val Apply(fun, _) = reify((null, null)).tree
    Apply.apply(fun, _1 :: _2 :: Nil)
  }

  def jObject(xs: Tree*) = {
    val Apply(fun, _) = reify(JObject()).tree
    Apply.apply(fun, xs.toList)
  }

  def jString(s: String) = {
    val Apply(fun, _) = reify(JString("")).tree
    Apply.apply(fun, literal(s).tree :: Nil)
  }

  def jDouble(n: Double) = {
    val Apply(fun, _) = reify(JDouble(0.0)).tree
    Apply.apply(fun, literal(n).tree :: Nil)
  }

  def bigInt(n: BigInt) = {
    val Apply(fun, _) = reify(BigInt("0")).tree
    Apply.apply(fun, literal(n.toString).tree :: Nil)
  }

  def jInt(n: BigInt) = {
    val Apply(fun, _) = reify(JInt(0)).tree
    Apply.apply(fun, bigInt(n) :: Nil)
  }

  def jBool(b: Boolean) = {
    if (b) {
      reify(JBool(true)).tree
    } else {
      reify(JBool(false)).tree
    }
  }

  def variable[T](id: String) = {
    context.Expr[T](Ident(newTermName(id))).tree
  }
}