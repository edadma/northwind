package xyz.hyperreal.northwind

import scala.util.parsing.input.{CharSequenceReader, Position, Positional}
import util.parsing.combinator.RegexParsers


object InsertParser {

	def parseStatement( statement: String ) = {
		val p = new InsertParser

		p.parseFromString( statement, p.inserts )
	}

}

class InsertParser extends RegexParsers {

	def pos = positioned( success(new Positional{}) ) ^^ { _.pos }

	def number = """\-?\d+(\.\d*)?""".r ^^ {
		case n if n contains '.' => FloatLit( n )
		case n => IntegerLit( n ) }

	def string = "\"" ~> """[^"\n]*""".r <~ "\"" ^^ StringLit

	def ident = pos ~ """[a-zA-Z_#$][a-zA-Z0-9_#$]*""".r ^^ { case p ~ n => Ident( p, n ) }

	def inserts = rep( insert )

	def insert =
		(("INSERT" ~ "INTO") ~> ident <~ ("VALUES" ~ "(")) ~ (repsep(value, ",") <~ (")" ~ ";")) ^^ {
			case table ~ values => Insert( table, values )
		}

	def value: Parser[Value] =
		number |
		string |
		"NULL" ^^^ NullLit

	def parseFromString[T]( src: String, grammar: Parser[T] ) = {
		parseAll( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

}

case class Ident( pos: Position, name: String )

trait Value
case class FloatLit( n: String ) extends Value
case class IntegerLit( n: String ) extends Value
case class StringLit( s: String ) extends Value
case object NullLit extends Value

case class Insert( table: Ident, values: List[Value] )