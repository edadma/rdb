package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{Position, Positional}

object JSONParser extends StandardTokenParsers with PackratParsers:
  class JSONLexer extends StdLexical:
    delimiters ++= Seq(
      ":",
      ",",
      "{",
      "}",
      "[",
      "]"
    )
    reserved ++= Seq("null")

    case class DecimalLit(chars: String) extends Token {
      override def toString: String = chars
    }

    override def token: Parser[Token] = decimalToken | super.token

    private def decimalToken: Parser[Token] =
      digits ~ '.' ~ digits ~ optExponent ^^ { case intPart ~ _ ~ fracPart ~ exp =>
        DecimalLit(s"$intPart.$fracPart$exp")
      } |
        '.' ~ digits ~ optExponent ^^ { case _ ~ fracPart ~ exp =>
          DecimalLit(s".$fracPart$exp")
        } |
        digits ~ exponent ^^ { case intPart ~ exp =>
          DecimalLit(s"$intPart$exp")
        }

    private def digits = rep1(digit) ^^ (_ mkString)

    private def chr(c: Char) = elem("", ch => ch == c)

    private def exponent = (chr('e') | 'E') ~ opt(chr('+') | '-') ~ digits ^^ {
      case e ~ None ~ exp    => List(e, exp) mkString
      case e ~ Some(s) ~ exp => List(e, s, exp) mkString
    }

    private def optExponent = opt(exponent) ^^ {
      case None    => ""
      case Some(e) => e
    }

  override val lexical: JSONLexer = new JSONLexer

  import lexical.DecimalLit

  def decimalLit: Parser[String] =
    elem("decimal", _.isInstanceOf[DecimalLit]) ^^ (_.asInstanceOf[DecimalLit].chars)

  type P[+T] = PackratParser[T]

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val json: P[Value] = arrayValue | objectValue

  lazy val value: P[Value] = arrayValue | objectValue | numberValue | stringValue | nullValue

  lazy val arrayValue: P[Value] =
    positioned(
      "[" ~> repsep(value, ",") <~ "]" ^^ (a => ArrayValue(a.toIndexedSeq))
    )

  lazy val property: P[(String, Value)] =
    stringLit ~ ":" ~ value ^^ { case k ~ _ ~ v =>
      k -> v
    }

  lazy val objectValue: P[Value] = "{" ~> repsep(property, ",") <~ "}" ^^ ObjectValue.apply

  lazy val numberValue: P[Value] =
    positioned(
      decimalLit ^^ (n => NumberValue(n.toDouble)) |
        numericLit ^^ (n => NumberValue(n.toInt))
    )

  lazy val stringValue: P[Value] = positioned(stringLit ^^ TextValue.apply)

  lazy val nullValue: P[Value] = positioned("null" ^^^ NullValue())

  def parseJSON(input: String): Value =
    val tokens = new PackratReader(new lexical.Scanner(input))

    phrase(json)(tokens) match
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)
