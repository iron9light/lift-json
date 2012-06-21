package net.liftweb.json
package macros

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author IL
 */
class JsonSuite extends FunSuite with ShouldMatchers {
  test("json DSL") {
    JSON( """{ "a": ["100", 100, 100.1, true, false, null] }""") should be ===
      JObject(
        ("a" -> JArray(JString("100") :: JInt(100) :: JDouble(100.1) :: JBool(true) :: JBool(false) :: JNull :: Nil))
      )
  }
}