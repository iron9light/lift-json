package net.liftweb.json.macros

import util.parsing.input.CharArrayReader._

/**
 * @author IL
 */
class Lexer extends scala.util.parsing.json.Lexer {
  override def token: Parser[Token] =
    ( string ^^ StringLit
    | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
    | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
    | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
    | number ^^ NumericLit
    | EofCh ^^^ EOF
    | delim
    | '\"' ~> failure("Unterminated string")
    | '\'' ~> failure("Unterminated string")
    | identChar ~ rep( identChar | digit ) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | failure("Illegal character")
    )

  override def string = super.string |
    '\'' ~> rep(charSeq | chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { _ mkString "" }
}
