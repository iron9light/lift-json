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

  sealed abstract class Token {
    var pos: Position = NoPosition
    def withPos(p: Position) = {
      pos = p
      this
    }
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
    val p = new Parser(new Buffer(parts, args))
    val token = p.nextToken
    token match {
      case End =>
        reify(JNothing)
      case _ =>
        val tree = astParse(token, p)
        p.nextToken match {
          case End =>
            context.Expr[JValue](tree)
          case badToken =>
            context.abort(badToken.pos, f"expected end, but got $badToken")
        }
    }

    // context.literalNull
  }

  private def astParse(p: Parser): Tree = {
    val token = p.nextToken
    astParse(token, p)
  }
  private def astParse(token: Token, p: Parser): Tree = {
    token match {
      case OpenFieldName =>
        astParseField(p, token.pos)
      case OpenObj =>
        astParseObj(p, token.pos)
      case OpenArr =>
        astParseArray(p, token.pos)
      case OpenString =>
        val stringTree = astParseString(p, token.pos)
        reify(JString(context.eval(context.Expr[String](stringTree)))).tree.setPos(stringTree.pos)
      case IntVal(value) =>
        reify(JInt(BigInt(context.eval(context.literal(value.toString))))).tree.setPos(token.pos)
      case DoubleVal(value) =>
        reify(JDouble(context.eval(context.literal(value)))).tree.setPos(token.pos)
      case BoolVal(true) =>
        reify(JTrue).tree.setPos(token.pos)
      case BoolVal(false) =>
        reify(JFalse).tree.setPos(token.pos)
      case NullVal =>
        reify(JNull).tree.setPos(token.pos)
      case ObjVal(x) =>
        x
      case _ =>
        context.abort(token.pos, f"wrong token: $token")
    }
  }

  private def astParseObj(p: Parser, startPosition: Position): Tree = {
    @tailrec
    def parseLoop(list: List[Tree]): Tree = {
      val token = p.nextToken
      token match {
        case OpenFieldName =>
          val feildTree = astParseField(p, token.pos)
          parseLoop(feildTree::list)
        case CloseObj =>
          val position = startPosition.withEnd(token.pos.endOrPoint)
          val Apply(fun, _) = reify(List(0)).tree
          val listExpr = context.Expr[List[JField]](Apply.apply(fun, list.reverse))
          reify(JObject(context.eval(listExpr))).tree.setPos(position)
        case _ =>
          context.abort(token.pos, f"expected feild, but got $token")
      }
    }

    parseLoop(Nil)
  }

  private def astParseField(p: Parser, startPosition: Position): Tree = {
    val token = p.nextToken
    val fieldNameTree = token match {
      case OpenString =>
        astParseString(p, token.pos)
      case ObjVal(x) =>
        x
      case _ =>
        context.abort(token.pos, f"expected field, array or object but got $token")

    }
    val fieldName = context.eval(context.Expr[String](fieldNameTree))

    p.nextToken match {
      case CloseFieldName =>
        val fieldValueTree = astParse(p)
        val fieldValue = context.eval(context.Expr[JValue](fieldValueTree))
        reify(JField(fieldName, fieldValue)).tree.setPos(startPosition.withEnd(fieldValueTree.pos.endOrPoint))
      case badToken =>
        context.abort(badToken.pos, f"expected close token for field name: $badToken")
    }
  }

  private def astParseArray(p: Parser, startPosition: Position): Tree = {
    @tailrec
    def parseLoop(list: List[Tree]): Tree = {
      val token = p.nextToken
      token match {
        case CloseArr =>
          val position = startPosition.withEnd(token.pos.endOrPoint)
          val Apply(fun, _) = reify(List(0)).tree
          val listExpr = context.Expr[List[JValue]](Apply.apply(fun, list.reverse))
          reify(JArray(context.eval(listExpr))).tree.setPos(position)
        case _ =>
          parseLoop(astParse(token, p)::list)
      }
    }

    parseLoop(Nil)
  }

  private def astParseString(p: Parser, startPosition: Position): Tree = {
    @tailrec
    def parseLoop(list: List[Tree]): Tree = {
      val token = p.nextToken
      token match {
        case StringVal(s) =>
          val tree = context.literal(s).tree.setPos(token.pos)
          parseLoop(tree::list)
        case ObjVal(x) =>
          parseLoop(x::list)
        case CloseString =>
          val Apply(fun, _) = reify(Array(0)).tree
          val position = startPosition.withEnd(token.pos.endOrPoint)
          val arrayExpr = context.Expr[Array[String]](Apply.apply(fun, list.reverse))
          reify(context.eval(arrayExpr).mkString).tree.setPos(position)
        case _ =>
          context.abort(token.pos, f"wrong token for string: $token")
      }
    }

    parseLoop(Nil)
  }

  private[interpolation] class Parser(buf: Buffer) {
    private[this] val blocks = new java.util.LinkedList[BlockMode]()
    private[this] var fieldNameMode = true
    private[this] var stringMode = false
    private[this] var stringBegin = true

    private[this] val tokenQueen = new java.util.LinkedList[Token]()

    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

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

      def parseString: Token = {
        val s = new java.lang.StringBuilder

        var c = buf.next
        val startPoint = buf.point
        var lastPoint = startPoint

        while (c != Left('"')) {
          if (c == Left('\\')) {
            c = buf.next
            c match {
              case Left(cc) =>
                cc match {
                  case '"' => s.append('"')
                  case '\\' => s.append('\\')
                  case '/' => s.append('/')
                  case 'b' => s.append('\b')
                  case 'f' => s.append('\f')
                  case 'n' => s.append('\n')
                  case 'r' => s.append('\r')
                  case 't' => s.append('\t')
                  case 'u' =>
                    val Left(c1) = buf.next
                    val Left(c2) = buf.next
                    val Left(c3) = buf.next
                    c = buf.next
                    val Left(c4) = c
                    val chars = Array(c1, c2, c3, c4)
                    val codePoint = Integer.parseInt(new String(chars), 16)
                    s.appendCodePoint(codePoint)
                  case _ => s.append('\\')
                }
              case Right(x) =>
                s.append('\\')
                val token = StringVal(s.toString).withPos(buf.position.withPoint(startPoint).withStart(startPoint).withEnd(lastPoint))
                tokenQueen.add(ObjVal(x))
                return token
            }
          } else {
            c match {
              case Left(cc) =>
                s.append(cc)
              case Right(x) =>
                tokenQueen.add(ObjVal(x))
                return StringVal(s.toString).withPos(buf.position.withPoint(startPoint).withStart(startPoint).withEnd(lastPoint))
            }
          }

          lastPoint = buf.point
          c = buf.next
        }

        stringMode = false
        stringBegin = true
        buf.eofIsFailure = false
        tokenQueen.add(CloseString.withPos(buf.position))
        StringVal(s.toString).withPos(buf.position.withPoint(startPoint).withStart(startPoint).withEnd(lastPoint))
      }

      if (!tokenQueen.isEmpty) {
        return tokenQueen.poll()
      }

      while (true) {
        if (stringMode) {
          if (stringBegin) {
            stringBegin = false
            buf.eofIsFailure = true
            return OpenString.withPos(buf.position)
          }

          return parseString
        } else {
          buf.next match {
            case Left(c) =>
              c match {
                case `EOF` =>
                  return End.withPos(buf.position)
                case '{' =>
                  blocks.addFirst(ObjectMode)
                  fieldNameMode = true
                  return OpenObj
                case '}' =>
                  blocks.poll()
                  return CloseObj
                case '"' =>
                  if (fieldNameMode && blocks.peek == ObjectMode) {
                    stringMode = true
                    return OpenFieldName
                  }
                  else {
                    stringMode = true
                  }
                case 't' =>
                  fieldNameMode = true
                  val startPoint = buf.point
                  if (buf.next == Left('r') && buf.next == Left('u') && buf.next == Left('e')) {
                    val endPoint = buf.point
                    val position = buf.position.withPoint(startPoint).withStart(startPoint).withEnd(endPoint)
                    return BoolVal(true).withPos(position)
                  }
                  context.abort(buf.position, "expected boolean")
                case 'f' =>
                  fieldNameMode = true
                  val startPoint = buf.point
                  if (buf.next == Left('a') && buf.next == Left('l') && buf.next == Left('s') && buf.next == Left('e')) {
                    val endPoint = buf.point
                    val position = buf.position.withPoint(startPoint).withStart(startPoint).withEnd(endPoint)
                    return BoolVal(false).withPos(position)
                  }
                  context.abort(buf.position, "expected boolean")
                case 'n' =>
                  fieldNameMode = true
                  val startPoint = buf.point
                  if (buf.next == Left('u') && buf.next == Left('l') && buf.next == Left('l')) {
                    val endPoint = buf.point
                    val position = buf.position.withPoint(startPoint).withStart(startPoint).withEnd(endPoint)
                    return NullVal.withPos(position)
                  }
                  context.abort(buf.position, "expected null")
                case ':' =>
                  if (blocks.peek == ArrayMode) {
                    context.abort(buf.position, "Colon in an invalid position")
                  }
                  fieldNameMode = false
                case '[' =>
                  blocks.addFirst(ArrayMode)
                  return OpenArr.withPos(buf.position)
                case ']' =>
                  fieldNameMode = true
                  blocks.poll()
                  return CloseArr.withPos(buf.position)
                case _ if Character.isDigit(c) || c == '-' =>
                  fieldNameMode = true
                  val startPoint = buf.point
                  val token = parseValue(c)
                  val endPoint = buf.point
                  val position = buf.position.withPoint(startPoint).withStart(startPoint).withEnd(endPoint)
                  return token.withPos(position)
                case _ if isDelimiter(c) =>
                case _ =>
                  context.abort(buf.position, f"unknown token $c")
              }
            case Right(x) =>
              if (fieldNameMode && blocks.peek == ObjectMode) {
                val token = OpenFieldName.withPos(x.pos.focusStart)
                tokenQueen.add(ObjVal(x))
                tokenQueen.add(CloseFieldName.withPos(x.pos.focusEnd))
                return token
              }
              else {
                fieldNameMode = true
                return ObjVal(x)
              }
          }
        }
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

    def position: Position = context.enclosingPosition.withPoint(point).withStart(point)

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
