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
import java.util

abstract class MacroJsonContextParser {
  val context: Context

  import context.universe._

  sealed abstract class Token {
    var pos: Position = NoPosition
  }

  case object OpenObj extends Token

  case object CloseObj extends Token

  case object OpenFieldName extends Token

  case object CloseFieldName extends Token

  case object OpenString extends Token

  case object CloseString extends Token

  case object End extends Token

  case class StringVal(value: String) extends Token

  case class IntVal(value: BigInt) extends Token

  case class DoubleVal(value: Double) extends Token

  case class BoolVal(value: Boolean) extends Token

  case class ObjVal(value: Tree) extends Token {
    pos = value.pos
  }

  case object NullVal extends Token

  case object OpenArr extends Token

  case object CloseArr extends Token

  private val EOF = (-1).asInstanceOf[Char]

  def parse(parts: Seq[Tree], args: Seq[Tree]): context.Expr[JValue] = {
    // todo
    context.literalNull
  }

  private[interpolation] class Parser(buf: Buffer) {
    private[this] val blocks = new java.util.LinkedList[BlockMode]()
    private[this] val fieldNameMode = true
    private[this] val stringMode = false

    private[this] val tokenQueen = new java.util.LinkedList[Token]()

    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseString: String = ???

      def parseValue(first: Char) = {
        var wasInt = true
        var doubleVal = false
        val s = new StringBuilder
        s.append(first)
        while (wasInt) {
          val Left(c) = buf.next
          if (c == '.' || c == 'e' || c == 'E') {
            doubleVal = true
            s.append(c)
          } else if (!(Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-')) {
            wasInt = false
            buf.back()
          } else s.append(c)
        }
        val value = s.toString()
        if (doubleVal) DoubleVal(parseDouble(value))
        else IntVal(BigInt(value))
      }

      while (true) {
        if (!tokenQueen.isEmpty) {
          return tokenQueen.poll()
        }
        // todo
      }

      End
    }

    sealed abstract class BlockMode

    case object ArrayMode extends BlockMode
    case object ObjectMode extends BlockMode
    case object StringMode extends BlockMode
  }

  // FIXME fail fast to prevent infinite loop, see
  // http://www.exploringbinary.com/java-hangs-when-converting-2-2250738585072012e-308/
  private val BrokenDouble = BigDecimal("2.2250738585072012e-308")

  private[interpolation] def parseDouble(s: String) = {
    val d = BigDecimal(s)
    if (d == BrokenDouble) sys.error("Error parsing 2.2250738585072012e-308")
    else d.doubleValue
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

    def back() {
      offset -= 1
    }

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
