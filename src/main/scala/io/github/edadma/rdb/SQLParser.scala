package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object SQLParser extends RegexParsers with PackratParsers {

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = s"(?i)$s\\b".r

  lazy val query: PackratParser[SQLSelectExpr] =
    kw("select") ~ expressions ~ "\u0004" ^^ { case _ ~ p ~ _ =>
      SQLSelectExpr(p to ArraySeq, Nil, None, null, None, null, None)
    }

  lazy val expressions: PackratParser[List[Expr]] = rep1sep(expression, ",")

  lazy val booleanExpression: PackratParser[Expr] = orExpression

  lazy val orExpression: PackratParser[Expr] = positioned(
    orExpression ~ kw("OR") ~ andExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "OR", r) } |
      andExpression
  )

  lazy val andExpression: PackratParser[Expr] = positioned(
    andExpression ~ kw("AND") ~ notExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "AND", r) } |
      notExpression
  )

  lazy val notExpression: PackratParser[Expr] = positioned(
    kw("NOT") ~> booleanPrimary ^^ (e => UnaryExpr("NOT", e)) | booleanPrimary
  )

  lazy val booleanPrimary: PackratParser[Expr] = positioned(
    expression ~ comparison ~ expression ^^ { case l ~ c ~ r => BinaryExpr(l, c, r) } |
      expression ~ ((kw("NOT") ~ kw("BETWEEN") ^^^ "NOT BETWEEN") | kw("BETWEEN")) ~ expression ~ kw(
        "AND"
      ) ~ expression ^^ { case e ~ b ~ l ~ _ ~ u =>
        BetweenExpr(e, b, l, u)
      } |
      expression ~ isNull ^^ { case e ~ n => UnaryExpr(n, e) } |
      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => SQLInArrayExpr(e, i, es) } |
      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => SQLInQueryExpr(e, i, q) } |
      kw("EXISTS") ~> "(" ~> query <~ ")" ^^ ExistsExpr.apply |
      booleanLiteral |
      "(" ~> booleanExpression <~ ")"
  )

  lazy val isNull: PackratParser[String] =
    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"

  lazy val in: PackratParser[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")

  lazy val comparison: PackratParser[String] =
    "<=" | ">=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE") | (kw(
      "NOT"
    ) ~ kw("ILIKE") ^^^ "NOT ILIKE")

  lazy val booleanLiteral: PackratParser[Expr] = positioned(
    (kw("TRUE") | kw("FALSE")) ^^ (s => BooleanExpr(s.equalsIgnoreCase("TRUE")))
  )

  lazy val expression: PackratParser[Expr] = additive

  lazy val additive: PackratParser[Expr] = positioned(
    additive ~ ("+" /* | "-"*/ ) ~ multiplicative ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      multiplicative
  )

  lazy val multiplicative: PackratParser[Expr] = positioned(
    positioned(
      multiplicative ~ ("*" | "/") ~ primary ^^ { case l ~ o ~ r =>
        BinaryExpr(l, o, r)
      } |
        primary
    )
  )

  lazy val literalExpression: PackratParser[Expr] =
    positioned(
      float ^^ NumberExpr.apply |
        integer ^^ NumberExpr.apply |
        stringLiteral |
        booleanLiteral
    )

  lazy val stringLiteral: PackratParser[Expr] = positioned(
    string ^^ (unescape _ andThen StringExpr.apply)
  )

//  lazy val pair: PackratParser[(String, Expr)] =
//    doubleQuoteString ~ ":" ~ (arrayExpression | objectExpression | literalExpression) ^^ { case k ~ _ ~ v =>
//      Pair(k, v)
//    }
//
//  lazy val arrayExpression: PackratParser[Expr] =
//    "[" ~> repsep(arrayExpression | objectExpression | literalExpression, ",") <~ "]" ^^ ArrayExpr
//
//  lazy val objectExpression: PackratParser[Expr] =
//    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpr
//
//  lazy val jsonExpression: PackratParser[Expr] =
//    (arrayExpression | objectExpression) ^^ JSONExpr

  lazy val applyExpression: PackratParser[Expr] =
    identifier ~ ("(" ~> expressions <~ ")") ^^ { case f ~ as => ApplyExpr(f, as) }

  lazy val primary: PackratParser[Expr] = positioned(
    literalExpression |
//        jsonExpression |
//      caseExpression |
//      applyExpression |
//      "-" ~> primary ^^ (e => UnaryExpr("-", e)) |
//      "(" ~> query <~ ")" ^^ SubqueryExpr.apply |
      "(" ~> expression <~ ")"
  )

//  lazy val caseExpression: PackratParser[CaseExpr] =
//    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ { case ws ~ e => CaseExpr(ws, e) }
//
//  lazy val when: PackratParser[OQLWhen] =
//    kw("WHEN") ~ booleanExpression ~ kw("THEN") ~ expression ^^ { case _ ~ l ~ _ ~ e => OQLWhen(l, e) }

  lazy val float: PackratParser[Number] = """[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?""".r ^^ (_.toDouble.asInstanceOf[Number])

  lazy val integer: PackratParser[Number] = "[0-9]+".r ^^ (_.toInt.asInstanceOf[Number])

  lazy val identifier: PackratParser[Ident] = positioned(
    """[a-zA-Z_$][a-zA-Z0-9_$]*""".r ^^ Ident.apply
  )

  lazy val singleQuoteString: PackratParser[String] =
    """'(?:[^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r ^^ (s => s.substring(1, s.length - 1))

  lazy val doubleQuoteString: PackratParser[String] =
    """"(?:[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ (s => s.substring(1, s.length - 1))

  lazy val string: PackratParser[String] = singleQuoteString | doubleQuoteString

  def parseQuery(input: String): SQLSelectExpr =
    parseAll(phrase(query), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)
    }

  def parseBooleanExpression(input: String): Expr =
    parseAll(phrase(booleanExpression), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)
    }

}
