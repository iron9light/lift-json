package net.liftweb.json
package macros

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author IL
 */
class JSuite extends FunSuite with ShouldMatchers {

  import JsonDSL._

  test("js DSL") {
    val a = 100
    val b = false
    val aName = "a.a"

    val actual = J( """
         {
           "a": {
             aName: [a, b, null, 100.1, 'yes', "'no'"]
           }
         }
                    """)

    val j_a: JValue = a
    val j_b: JValue = b
    val expected: JValue = ("a" -> (
      aName -> (j_a :: j_b :: JNull :: JDouble(100.1) :: JString("yes") :: JString("'no'") :: Nil)
      ))
    actual should be === expected
  }
}
