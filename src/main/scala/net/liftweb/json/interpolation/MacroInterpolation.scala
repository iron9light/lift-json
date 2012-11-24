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

import reflect.macros.Context

object MacroInterpolation {
  def interpolate(c: Context)(args: c.Expr[_]*): c.Expr[JValue] = {
    import c.universe._
    val parts = c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, literalParts))), _) =>
        literalParts
      case _ =>
    }

    c.literalNull
  }
}