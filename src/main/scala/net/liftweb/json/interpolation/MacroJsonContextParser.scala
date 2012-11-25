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
import annotation.tailrec

abstract class MacroJsonContextParser {
  val context: Context

  import context.universe._

  private val EOF = (-1).asInstanceOf[Char]

  def parse(parts: Seq[Tree], args: Seq[Tree]): context.Expr[JValue] = {
    // todo
    context.literalNull
  }

  private[interpolation] class Buffer(parts: Seq[Tree], args: Seq[Tree]) {
    private[this] val partsIter = parts.iterator
    private[this] val argsIter = parts.iterator

    private[this] var current: Option[String] = None
    private[this] var offset = 0
    private[this] var start = context.enclosingPosition.startOrPoint
    private[this] var isPart = true

    {
      if (partsIter.hasNext) {
        val tree = partsIter.next()
        val Literal(Constant(s: String)) = tree
        current = Some(s)
        start = tree.pos.startOrPoint
      }
    }

    var eofIsFailure = false

    def point: Int = start + offset

    @tailrec
    final def next: Either[Char, Tree] = {
      if (isPart) {
        current match {
          case None =>
            if (eofIsFailure) {
              context.abort(context.enclosingPosition.withPoint(point), "unexpected eof")
            } else {
              Left(EOF)
            }
          case Some(s) if s.length <= offset =>
            isPart = false
            next
          case Some(s) =>
            val c = s(offset)
            offset += 1
            Left(c)
        }
      } else {
        isPart = true
        val argTree = argsIter.next()
        if (partsIter.hasNext) {
          val tree = partsIter.next()
          val Literal(Constant(s: String)) = tree
          current = Some(s)
          start = tree.pos.startOrPoint
          offset = 0
        } else {
          current = None
          start = argTree.pos.endOrPoint
          offset = 0
        }
        Right(argTree)
      }
    }
  }

}
