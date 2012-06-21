package net.liftweb.json
package macros

import scala.reflect.makro.Context

/**
 * @author IL
 */
private[macros] object MacroHelper {
  def list(c: Context)(xs: c.Tree*) = {
    import c.universe._
    val Apply(fun, _) = reify(List(0)).tree
    Apply.apply(fun, xs.toList)
  }

  def jArray(c: Context)(xs: c.Tree*) = {
    import c.universe._
    val Apply(fun, _) = reify(JArray(Nil)).tree
    Apply.apply(fun, list(c)(xs: _*) :: Nil)
  }

  def pair(c: Context)(_1: c.Tree, _2: c.Tree) = {
    import c.universe._
    val Apply(fun, _) = reify((null, null)).tree
    Apply.apply(fun, _1 :: _2 :: Nil)
  }

  def jObject(c: Context)(xs: c.Tree*) = {
    import c.universe._
    val Apply(fun, _) = reify(JObject()).tree
    Apply.apply(fun, xs.toList)
  }

  def jString(c: Context)(s: String) = {
    import c.universe._
    val Apply(fun, _) = reify(JString("")).tree
    Apply.apply(fun, c.literal(s).tree :: Nil)
  }

  def jDouble(c: Context)(n: Double) = {
    import c.universe._
    val Apply(fun, _) = reify(JDouble(0.0)).tree
    Apply.apply(fun, c.literal(n).tree :: Nil)
  }

  def bigInt(c: Context)(n: BigInt) = {
    import c.universe._
    val Apply(fun, _) = reify(BigInt("0")).tree
    Apply.apply(fun, c.literal(n.toString).tree :: Nil)
  }

  def jInt(c: Context)(n: BigInt) = {
    import c.universe._
    val Apply(fun, _) = reify(JInt(0)).tree
    Apply.apply(fun, bigInt(c)(n) :: Nil)
  }

  def jBool(c: Context)(b: Boolean) = {
    if (b) {
      c.reify(JBool(true)).tree
    } else {
      c.reify(JBool(false)).tree
    }
  }

  def variable[T](c: Context)(id: String) = {
    import c.universe._
    c.Expr[T](Ident(newTermName(id))).tree
  }
}