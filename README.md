lift-json for scala 2.10
========================

Based on lift branch [joni_scala_2.10](https://github.com/lift/framework/tree/joni_scala_2.10)

Try to add some cool things with new features of scala 2.10 (reflection and macro etc.)

### Dynamic invoke

https://github.com/iron9light/lift-json/blob/master/src/test/scala/net/liftweb/json/JValueSuite.scala

```scala
val j: JValue = (
  "a" -> (
    "b" -> "c"
    )
  )

j.a.b should be === JString("c")
j.b should be === JNothing
```

### Write JValue with JSON String

https://github.com/iron9light/lift-json/blob/master/src/test/scala/net/liftweb/json/macros/JsonSuite.scala#L11

Kinda external DSL val scala macro.
So this JSON string will be compiled at compile-time.

```scala
JSON( """{ "a": ["100", 100, 100.1, true, false, null] }""") should be ===
  JObject(
    ("a" -> JArray(JString("100") :: JInt(100) :: JDouble(100.1) :: JBool(true) :: JBool(false) :: JNull :: Nil))
  )
```

### Write JValue with Javascript-like String

https://github.com/iron9light/lift-json/blob/master/src/test/scala/net/liftweb/json/macros/JSuite.scala

Same as the JSON String, but support variables.

```scala
import JsonDSL._
val a = 100
val b = false
val aName = "a.a"
J( """
{
  "a": {
    aName: [a, b, null, 100.1, 'yes', "'no'"]
  }
}
""")
```

### And more...