/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.json
package interpolation

import org.specs2.mutable.Specification

object MacroJsonContextExample extends Specification {
  import JsonDSL._

  "MacroJsonContext example" in {
    val firstName: JValue = "Iron"
    val secondName: JValue = "Light"
    val name = "Name"
    val json: JValue = J"""
    {
      "firstName": $firstName,
      "last$name": $secondName,
      "age": 5
    }
    """

    json mustEqual null
  }
}
