I'm an APP now
==============

You guys only love app, no matter how cool I am.
So I took a *Reassignment surgery* to be an app. Am I sexy enough now?!

How To Run
----------

`sbt run`

Warnning
--------
There's a **scala compiler bug** for you.
The compile will be failed at the first time with the error shows below:

> [error] macro implementation not found: J (the most common reason for that is that you cannot use macro implementations in the same compilation run that defines them)

You need to remove the "J" [here](https://github.com/iron9light/lift-json/blob/contest/src/main/scala/net/liftweb/json/app/MyApp.scala#L14) and compile. Then you restore the "J" and compile again. It will be success this time.

Why I take part in this contest?
--------------------------------

I just wanna show you my code, and say **Thanks** for all the guys behind scala.
And, yes, I want a T-Shirt.



lift-json for scala 2.10
========================

This project is an expriment and preview for next version [life](http://liftweb.net)-json library.

Try to add some cool things with new features of scala 2.10 (string interpolation and macro etc.)

This branch is focus on the best way to create a Json object, with string interpolation and macro.
Things can't be more easier!

Features
--------

### Create Json via String Interpolation at runtime

https://github.com/iron9light/lift-json/blob/interpolation/src/test/scala/net/liftweb/json/interpolation/JsonContextExample.scala#L29

Kinda external DSL val scala string interpolation.

This JSON string will be parsed at runtime.

```scala
    val firstName: JValue = "Iron"
    val secondName: JValue = "Light"
    val name = "Name"
    val json = json"""
    {
      "firstName": $firstName,
      "last$name": $secondName,
      "age": 5
    }
    """
```

### Create Json via String Interpolation at compile-time

https://github.com/iron9light/lift-json/blob/interpolation/src/test/scala/net/liftweb/json/interpolation/MacroJsonContextExample.scala#L29

Same as above, except it's created at compile-time!

Typesafe and great runtime efficiency.

All the **MACRO** style!

```scala
    val firstName = "Iron"
    val secondName = "Light"
    val name = "Name"
    val json: JValue = J"""
    {
      "firstName": $firstName,
      "last$name": $secondName,
      "age": 5
    }
    """
```

Build
-----

### Prerequired

* Java JDK 1.6+

* sbt 0.12.1+

### Compile & Test

`sbt compile`

`sbt test`

License
-------

[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

About Me
--------

I'm a committer of [Lift](http://liftweb.net) framework.

And now I'm working for Microsoft.

twitter: [@iron9light](https://twitter.com/iron9light)