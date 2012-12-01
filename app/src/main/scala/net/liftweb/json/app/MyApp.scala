package net.liftweb.json
package app

import interpolation._
import net.liftweb.json.JsonDSL._

object MyApp extends App {
  val whatIAm = "library"
  val whatIMustBe = "application"
  val whoIsMyFather = "IL"
  val whatMyFatherWant = "T-Shirt"
  val smile = "(^_^)"

  val json = J"""
    {
      "soul": $whatIAm,
      "body $smile": $whatIMustBe,
      "desire": $whatMyFatherWant,
      "auther": $whoIsMyFather
    }
    """

  printJson(json)

  def printJson(json: JValue) {
    println(pretty(render(json)))
  }
}
