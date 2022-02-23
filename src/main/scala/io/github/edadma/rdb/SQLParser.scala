package io.github.edadma.rdb

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object SQLParser extends RegexParsers with PackratParsers {

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = (s"(?i)$s\\b").r

  lazy val query: PackratParser[OQLQuery] =
    entityName ~ not(".") ~ project ~ opt(select) ~ opt(group) ~ opt(order) ~ restrict ^^ {
      case e ~ _ ~ p ~ s ~ g ~ o ~ Seq(lim, off) => OQLQuery(e, null, null, p, s, g, o, lim, off)
    }

  lazy val expressions: PackratParser[List[OQLExpression]] = rep1sep(expression, ",")

  lazy val booleanExpression: PackratParser[OQLExpression] = orExpression

  lazy val orExpression: PackratParser[OQLExpression] =
    orExpression ~ kw("OR") ~ andExpression ^^ { case l ~ _ ~ r => InfixOQLExpression(l, "OR", r) } |
      andExpression

  lazy val andExpression: PackratParser[OQLExpression] =
    andExpression ~ kw("AND") ~ notExpression ^^ { case l ~ _ ~ r => InfixOQLExpression(l, "AND", r) } |
      notExpression

  lazy val notExpression: PackratParser[OQLExpression] =
    kw("NOT") ~> booleanPrimary ^^ (e => PrefixOQLExpression("NOT", e)) | booleanPrimary

  lazy val booleanPrimary: PackratParser[OQLExpression] =
    expression ~ comparison ~ expression ^^ { case l ~ c ~ r => InfixOQLExpression(l, c, r) } |
      expression ~ ((kw("NOT") ~ kw("BETWEEN") ^^^ "NOT BETWEEN") | kw("BETWEEN")) ~ expression ~ kw(
        "AND"
      ) ~ expression ^^ { case e ~ b ~ l ~ _ ~ u =>
        BetweenOQLExpression(e, b, l, u)
      } |
      expression ~ isNull ^^ { case e ~ n => PostfixOQLExpression(e, n) } |
      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => InArrayOQLExpression(e, i, es) } |
      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => InQueryOQLExpression(e, i, q) } |
      kw("EXISTS") ~> "(" ~> query <~ ")" ^^ ExistsOQLExpression |
      booleanLiteral |
      qualifiedAttributeExpression |
      "(" ~> booleanExpression <~ ")" ^^ GroupedOQLExpression

  lazy val isNull: PackratParser[String] =
    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"

  lazy val in: PackratParser[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")

  lazy val comparison: PackratParser[String] =
    "<=" | ">=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE") | (kw(
      "NOT"
    ) ~ kw("ILIKE") ^^^ "NOT ILIKE")

  lazy val booleanLiteral: PackratParser[OQLExpression] =
    (kw("TRUE") | kw("FALSE") | kw("NULL")) ^^ BooleanOQLExpression

  lazy val expression: PackratParser[OQLExpression] = additive

  lazy val additive: PackratParser[OQLExpression] =
    additive ~ ("+" | "-") ~ multiplicative ^^ { case l ~ o ~ r =>
      InfixOQLExpression(l, o, r)
    } |
      multiplicative

  lazy val multiplicative: PackratParser[OQLExpression] =
    multiplicative ~ ("*" | "/") ~ primary ^^ { case l ~ o ~ r =>
      InfixOQLExpression(l, o, r)
    } |
      primary

  lazy val literalExpression: PackratParser[OQLExpression] =
    float ^^ FloatOQLExpression |
      integer ^^ IntegerOQLExpression |
      stringLiteral |
      booleanLiteral

  lazy val stringLiteral: PackratParser[OQLExpression] =
    string ^^ (unescape _ andThen StringOQLExpression)

  lazy val pair: PackratParser[(String, OQLExpression)] =
    doubleQuoteString ~ ":" ~ (arrayExpression | objectExpression | literalExpression) ^^ { case k ~ _ ~ v =>
      (k, v)
    }

  lazy val arrayExpression: PackratParser[OQLExpression] =
    "[" ~> repsep(arrayExpression | objectExpression | literalExpression, ",") <~ "]" ^^ ArrayOQLExpression

  lazy val objectExpression: PackratParser[OQLExpression] =
    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectOQLExpression

  lazy val jsonExpression: PackratParser[OQLExpression] =
    (arrayExpression | objectExpression) ^^ JSONOQLExpression

  lazy val primary: PackratParser[OQLExpression] =
    literalExpression |
      jsonExpression |
      starExpression |
      caseExpression |
      applyExpression |
      qualifiedAttributeExpression |
      "&" ~> identifiers ^^ (ReferenceOQLExpression(_)) |
      "-" ~> primary ^^ (e => PrefixOQLExpression("-", e)) |
      "(" ~> query <~ ")" ^^ QueryOQLExpression |
      "(" ~> expression <~ ")" ^^ GroupedOQLExpression

  lazy val caseExpression: PackratParser[CaseOQLExpression] =
    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ { case ws ~ e => CaseOQLExpression(ws, e) }

  lazy val when: PackratParser[OQLWhen] =
    kw("WHEN") ~ booleanExpression ~ kw("THEN") ~ expression ^^ { case _ ~ l ~ _ ~ e => OQLWhen(l, e) }

  lazy val float: PackratParser[Double] = """[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?""".r ^^ (_.toDouble)

  lazy val integer: PackratParser[Int] = "[0-9]+".r ^^ (_.toInt)

  lazy val identifier: PackratParser[Ident] =
    pos ~ """[a-zA-Z_$][a-zA-Z0-9_$]*""".r ^^ { case p ~ s =>
      Ident(s, p)
    }

  lazy val singleQuoteString: PackratParser[String] =
    """'(?:[^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r ^^ (s => s.substring(1, s.length - 1))

  lazy val doubleQuoteString: PackratParser[String] =
    """"(?:[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ (s => s.substring(1, s.length - 1))

  lazy val string: PackratParser[String] = singleQuoteString | doubleQuoteString

  def parseQuery(input: String): OQLQuery =
    parseAll(phrase(query), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error, input)
    }

  def parseBooleanExpression(input: String): OQLExpression =
    parseAll(phrase(booleanExpression), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error, input)
    }

}
