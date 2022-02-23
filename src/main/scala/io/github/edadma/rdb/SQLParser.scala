package io.github.edadma.rdb

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object SQLParser extends RegexParsers with PackratParsers {

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = s"(?i)$s\\b".r

  lazy val query: PackratParser[SQLSelectExpr] =
    kw("select") ~ expressions ~ opt(select) ~ opt(group) ~ opt(order) ~ restrict ^^ {
      case e ~ _ ~ p ~ s ~ g ~ o ~ Seq(lim, off) => SQLSelectExpr(e, null, null, p, s, g, o, lim, off)
    }

  lazy val expressions: PackratParser[List[Expr]] = rep1sep(expression, ",")

  lazy val booleanExpression: PackratParser[Expr] = orExpression

  lazy val orExpression: PackratParser[Expr] =
    orExpression ~ kw("OR") ~ andExpression ^^ { case l ~ _ ~ r => BinaryExpr(l.pos, l, "OR", r) } |
      andExpression

  lazy val andExpression: PackratParser[Expr] =
    andExpression ~ kw("AND") ~ notExpression ^^ { case l ~ _ ~ r => BinaryExpr(l.pos, l, "AND", r) } |
      notExpression

  lazy val notExpression: PackratParser[Expr] =
    kw("NOT") ~> booleanPrimary ^^ (e => UnaryExpr("NOT", e)) | booleanPrimary

  lazy val booleanPrimary: PackratParser[Expr] =
    expression ~ comparison ~ expression ^^ { case l ~ c ~ r => BinaryExpr(l, c, r) } |
      expression ~ ((kw("NOT") ~ kw("BETWEEN") ^^^ "NOT BETWEEN") | kw("BETWEEN")) ~ expression ~ kw(
        "AND"
      ) ~ expression ^^ { case e ~ b ~ l ~ _ ~ u =>
        BetweenExpr(e, b, l, u)
      } |
      expression ~ isNull ^^ { case e ~ n => UnaryExpr(e.pos, n, e) } |
      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => InExpr(e.pos, e, i, es) } |
      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => InQueryExpr(e, i, q) } |
      kw("EXISTS") ~> "(" ~> query <~ ")" ^^ ExistsExpr |
      booleanLiteral |
      qualifiedAttributeExpression |
      "(" ~> booleanExpression <~ ")" ^^ GroupedExpr

  lazy val isNull: PackratParser[String] =
    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"

  lazy val in: PackratParser[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")

  lazy val comparison: PackratParser[String] =
    "<=" | ">=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE") | (kw(
      "NOT"
    ) ~ kw("ILIKE") ^^^ "NOT ILIKE")

  lazy val booleanLiteral: PackratParser[Expr] =
    (kw("TRUE") | kw("FALSE") | kw("NULL")) ^^ BooleanExpr

  lazy val expression: PackratParser[Expr] = additive

  lazy val additive: PackratParser[Expr] =
    additive ~ ("+" | "-") ~ multiplicative ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      multiplicative

  lazy val multiplicative: PackratParser[Expr] =
    multiplicative ~ ("*" | "/") ~ primary ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      primary

  lazy val literalExpression: PackratParser[Expr] =
    float ^^ FloatExpr |
      integer ^^ IntegerExpr |
      stringLiteral |
      booleanLiteral

  lazy val stringLiteral: PackratParser[Expr] =
    string ^^ (unescape _ andThen StringExpr)

  lazy val pair: PackratParser[(String, Expr)] =
    doubleQuoteString ~ ":" ~ (arrayExpression | objectExpression | literalExpression) ^^ { case k ~ _ ~ v =>
      (k, v)
    }

  lazy val arrayExpression: PackratParser[Expr] =
    "[" ~> repsep(arrayExpression | objectExpression | literalExpression, ",") <~ "]" ^^ ArrayExpr

  lazy val objectExpression: PackratParser[Expr] =
    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpr

  lazy val jsonExpression: PackratParser[Expr] =
    (arrayExpression | objectExpression) ^^ JSONExpr

  lazy val primary: PackratParser[Expr] =
    literalExpression |
      jsonExpression |
      starExpression |
      caseExpression |
      applyExpression |
      qualifiedAttributeExpression |
      "-" ~> primary ^^ (e => UnaryExpr("-", e)) |
      "(" ~> query <~ ")" ^^ SubqueryExpr |
      "(" ~> expression <~ ")" ^^ GroupedExpr

  lazy val caseExpression: PackratParser[CaseExpr] =
    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ { case ws ~ e => CaseExpr(ws, e) }

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

  def parseQuery(input: String): SQLSelectExpr =
    parseAll(phrase(query), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error, input)
    }

  def parseBooleanExpression(input: String): Expr =
    parseAll(phrase(booleanExpression), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error, input)
    }

}
