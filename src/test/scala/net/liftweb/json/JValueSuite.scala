package net.liftweb.json

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author IL
 */
class JValueSuite extends FunSuite with ShouldMatchers {

  import JsonDSL._

  test("dynamic invoke") {
    val j: JValue = (
      "a" -> (
        "b" -> "c"
        )
      )

    j.a.b should be === JString("c")

    j.b should be === JNothing
  }
}
