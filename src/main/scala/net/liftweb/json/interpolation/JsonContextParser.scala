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

import annotation.tailrec
import java.util

object JsonContextParser {

  class ParseException(message: String, cause: Exception) extends Exception(message, cause)

  sealed abstract class Token

  case object OpenObj extends Token

  case object CloseObj extends Token

  case class FieldStart(name: String) extends Token

  case object End extends Token

  case class StringVal(value: String) extends Token

  case class IntVal(value: BigInt) extends Token

  case class DoubleVal(value: Double) extends Token

  case class BoolVal(value: Boolean) extends Token

  case class ObjVal(value: Any) extends Token

  case object NullVal extends Token

  case object OpenArr extends Token

  case object CloseArr extends Token

  private val EOF = (-1).asInstanceOf[Char]

  def parse(parts: Seq[String], args: Seq[_]): JValue = {
    parse(new Buffer(parts, args))
  }

  private def parse(buf: Buffer): JValue = {
    try {
      astParser(new Parser(buf))
    } catch {
      case e: ParseException => throw e
      case e: Exception => throw new ParseException("parsing failed", e)
    }
  }

  class Parser(buf: Buffer) {


    private[this] val blocks = new util.LinkedList[BlockMode]()
    private[this] var fieldNameMode = true

    def fail(msg: String) = throw new ParseException(msg + "\nNear: " + buf.near, null)

    /** Parse next Token from stream.
      */
    def nextToken: Token = {

      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseString: String =
        try {
          unquote(buf)
        } catch {
          case p: ParseException => throw p
          case _ => fail("unexpected string end")
        }

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
        buf.next match {
          case Left(c) =>
            c match {
              case `EOF` =>
//                buf.automaticClose
                return End
              case '{' =>
                blocks.addFirst(OBJECT)
                fieldNameMode = true
                return OpenObj
              case '}' =>
                blocks.poll
                return CloseObj
              case '"' =>
                if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseString)
                else {
                  fieldNameMode = true
                  return StringVal(parseString)
                }
              case 't' =>
                fieldNameMode = true
                if (buf.next == Left('r') && buf.next == Left('u') && buf.next == Left('e')) {
                  return BoolVal(true)
                }
                fail("expected boolean")
              case 'f' =>
                fieldNameMode = true
                if (buf.next == Left('a') && buf.next == Left('l') && buf.next == Left('s') && buf.next == Left('e')) {
                  return BoolVal(false)
                }
                fail("expected boolean")
              case 'n' =>
                fieldNameMode = true
                if (buf.next == Left('u') && buf.next == Left('l') && buf.next == Left('l')) {
                  return NullVal
                }
                fail("expected null")
              case ':' =>
                if (blocks.peek == ARRAY) fail("Colon in an invalid position")
                fieldNameMode = false
              case '[' =>
                blocks.addFirst(ARRAY)
                return OpenArr
              case ']' =>
                fieldNameMode = true
                blocks.poll
                return CloseArr
              case c if Character.isDigit(c) || c == '-' =>
                fieldNameMode = true
                return parseValue(c)
              case c if isDelimiter(c) =>
              case c => fail("unknown token " + c)
            }
          case Right(x) =>
            if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(x.toString)
            else {
              fieldNameMode = true
              return ObjVal(x)
            }
        }
      }

//      buf.automaticClose
      End
    }

    sealed abstract class BlockMode

    case object ARRAY extends BlockMode

    case object OBJECT extends BlockMode

  }

  private[json] def unquote(buf: Buffer): String = {
    buf.eofIsFailure = true
    // buf.mark()

    val s = new java.lang.StringBuilder

    var c = buf.next

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
            s.append(x)
        }
      } else {
        c match {
          case Left(cc) =>
            s.append(cc)
          case Right(x) =>
            s.append(x)
        }
      }

      c = buf.next
    }

    buf.eofIsFailure = false
    s.toString
  }

  // FIXME fail fast to prevent infinite loop, see
  // http://www.exploringbinary.com/java-hangs-when-converting-2-2250738585072012e-308/
  private val BrokenDouble = BigDecimal("2.2250738585072012e-308")

  private[json] def parseDouble(s: String) = {
    val d = BigDecimal(s)
    if (d == BrokenDouble) sys.error("Error parsing 2.2250738585072012e-308")
    else d.doubleValue
  }

  private val astParser = (p: Parser) => {
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    // This is a slightly faster way to correct order of fields and arrays than using 'map'.
    def reverse(v: JValue): JValue = v match {
      case JObject(l) => JObject((l.map { case (n, v) => (n, reverse(v)) }).reverse)
      case JArray(l) => JArray(l.map(reverse).reverse)
      case x => x
    }

    def closeBlock(v: Any) {
      @inline def toJValue(x: Any) = x match {
        case json: JValue => json
        case _ => p.fail("unexpected field " + x)
      }

      vals.peekOption match {
        case Some((name: String, value)) =>
          vals.pop(classOf[JField])
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject((name, toJValue(v)) :: obj.obj))
        case Some(o: JObject) =>
          vals.replace(JObject(vals.peek(classOf[JField]) :: o.obj))
        case Some(a: JArray) => vals.replace(JArray(toJValue(v) :: a.arr))
        case Some(x) => p.fail("expected field, array or object but got " + x)
        case None => root = Some(reverse(toJValue(v)))
      }
    }

    def newValue(v: JValue) {
      vals.peekAny match {
        case (name: String, value) =>
          vals.pop(classOf[JField])
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject((name, v) :: obj.obj))
        case a: JArray => vals.replace(JArray(v :: a.arr))
        case _ => p.fail("expected field or array")
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(JObject(Nil))
        case FieldStart(name) => vals.push(JField(name, null))
        case StringVal(x)     => newValue(JString(x))
        case IntVal(x)        => newValue(JInt(x))
        case DoubleVal(x)     => newValue(JDouble(x))
        case BoolVal(x)       => newValue(JBool(x))
        case NullVal          => newValue(JNull)
        case ObjVal(x: JValue)=> newValue(x)
        case CloseObj         => closeBlock(vals.popAny)
        case OpenArr          => vals.push(JArray(Nil))
        case CloseArr         => closeBlock(vals.pop(classOf[JArray]))
        case End              =>
      }
    } while (token != End)

    root getOrElse JNothing
  }

  private class ValStack(parser: Parser) {
    import java.util.LinkedList
    private[this] val stack = new util.LinkedList[Any]()

    def popAny = stack.poll
    def pop[A](expectedType: Class[A]) = convert(stack.poll, expectedType)
    def push(v: Any) = stack.addFirst(v)
    def peekAny = stack.peek
    def peek[A](expectedType: Class[A]) = convert(stack.peek, expectedType)
    def replace[A](newTop: Any) = stack.set(0, newTop)

    private def convert[A](x: Any, expectedType: Class[A]): A = {
      if (x == null) parser.fail("expected object or array")
      try { x.asInstanceOf[A] } catch { case _: ClassCastException => parser.fail("unexpected " + x) }
    }

    def peekOption = if (stack isEmpty) None else Some(stack.peek)
  }

  private[interpolation] class Buffer(parts: Seq[String], args: Seq[_]) {
    private[this] val partsIter = parts.iterator
    private[this] val argsIter = args.iterator

    private[this] var current: Option[Either[String, _]] = _
    read(false)

    var curMark = -1
    var eofIsFailure = false
    private[this] var cur = 0

    def mark() {
      curMark = cur
    }

    def back() {
      cur -= 1
    }

    def near: String = {
      // todo
      ""
    }

    private[this] def read(currentIsPart: Boolean) {
      if (currentIsPart) {
        if (argsIter.hasNext) {
          current = Some(Right(argsIter.next()))
        } else {
          current = None
        }
      } else {
        if (partsIter.hasNext) {
          current = Some(Left(partsIter.next()))
        } else {
          // todo
          throw new Exception
        }
      }

      curMark = -1
      cur = 0
    }

    @tailrec
    final def next: Either[Char, _] = {
      current match {
        case None =>
          if (eofIsFailure) {
            // todo
            throw new Exception("unexpected eof")
          } else {
            Left(EOF)
          }
        case Some(Left(s)) =>
          if (cur == s.length) {
            read(true)
            next
          } else {
            val c = s.charAt(cur)
            cur += 1
            Left(c)
          }
        case Some(Right(x)) =>
          read(false)
          Right(x)
      }
    }

    def substring = {
      val Some(Left(s)) = current
      s.substring(curMark, cur - 1)
    }
  }

}
