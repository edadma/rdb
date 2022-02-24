package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.{Position, Positional}

object SQLParser extends StdTokenParsers with PackratParsers:

  type Tokens = StdLexical

  val lexical: StdLexical =
    new StdLexical:
      delimiters ++= Seq("+", "-", "*", "/", "(", ")", ".")
      reserved ++= Seq("SELECT", "FROM")

      override def token: Parser[Token] =
        quotedToken | super.token

      private def quotedToken: Parser[Token] =
        '"' ~> rep(guard(not('"')) ~> elem("", _ => true)) <~ '"' ^^ { l => Identifier(l mkString) }

  type P[+T] = PackratParser[T]

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val query: P[SQLSelectExpr] =
    "SELECT" ~ expressions ~ "FROM" ~ repsep(source, ",") ^^ { case _ ~ p ~ _ ~ f =>
      SQLSelectExpr(p to ArraySeq, f, None, null, None, null, None)
    }

  lazy val source: P[Expr] = (table | ("(" ~> query <~ ")")) ~ opt(identifier) ^^ {
    case s ~ None    => s
    case s ~ Some(a) => AliasOperator(s, a)
  }

  lazy val table: P[Expr] = positioned(
    identifier ^^ TableOperator.apply
  )

  lazy val star: P[Expr] = positioned(
    "*" ^^^ StarExpr()
  )

  lazy val identifier: P[Ident] = positioned(
    ident ^^ Ident.apply
  )

  lazy val expressions: P[Seq[Expr]] = rep1sep(expression | star, ",")

//  lazy val booleanExpression: P[Expr] = orExpression
//
//  lazy val orExpression: P[Expr] = positioned(
//    orExpression ~ kw("OR") ~ andExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "OR", r) } |
//      andExpression
//  )
//
//  lazy val andExpression: P[Expr] = positioned(
//    andExpression ~ kw("AND") ~ notExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "AND", r) } |
//      notExpression
//  )
//
//  lazy val notExpression: P[Expr] = positioned(
//    kw("NOT") ~> booleanPrimary ^^ (e => UnaryExpr("NOT", e)) | booleanPrimary
//  )
//
//  lazy val booleanPrimary: P[Expr] = positioned(
//    expression ~ comparison ~ expression ^^ { case l ~ c ~ r => BinaryExpr(l, c, r) } |
//      expression ~ ((kw("NOT") ~ kw("BETWEEN") ^^^ "NOT BETWEEN") | kw("BETWEEN")) ~ expression ~ kw(
//        "AND"
//      ) ~ expression ^^ { case e ~ b ~ l ~ _ ~ u =>
//        BetweenExpr(e, b, l, u)
//      } |
//      expression ~ isNull ^^ { case e ~ n => UnaryExpr(n, e) } |
//      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => SQLInArrayExpr(e, i, es) } |
//      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => SQLInQueryExpr(e, i, q) } |
//      kw("EXISTS") ~> "(" ~> query <~ ")" ^^ ExistsExpr.apply |
//      booleanLiteral |
//      "(" ~> booleanExpression <~ ")"
//  )
//
//  lazy val isNull: P[String] =
//    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"
//
//  lazy val in: P[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")
//
//  lazy val comparison: P[String] =
//    "<=" | ">=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE") | (kw(
//      "NOT"
//    ) ~ kw("ILIKE") ^^^ "NOT ILIKE")
//
//  lazy val booleanLiteral: P[Expr] = positioned(
//    (kw("TRUE") | kw("FALSE")) ^^ (s => BooleanExpr(s.equalsIgnoreCase("TRUE")))
//  )

  lazy val expression: P[Expr] = additive

  lazy val additive: P[Expr] = positioned(
    additive ~ ("+" | "-") ~ multiplicative ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      multiplicative
  )

  lazy val multiplicative: P[Expr] = positioned(
    positioned(
      multiplicative ~ ("*" | "/") ~ primary ^^ { case l ~ o ~ r =>
        BinaryExpr(l, o, r)
      } |
        primary
    )
  )

//  lazy val stringLiteral: P[Expr] = positioned(
//    string ^^ (unescape _ andThen StringExpr.apply)
//  )

//  lazy val pair: P[(String, Expr)] =
//    doubleQuoteString ~ ":" ~ (arrayExpression | objectExpression | literalExpression) ^^ { case k ~ _ ~ v =>
//      Pair(k, v)
//    }
//
//  lazy val arrayExpression: P[Expr] =
//    "[" ~> repsep(arrayExpression | objectExpression | literalExpression, ",") <~ "]" ^^ ArrayExpr
//
//  lazy val objectExpression: P[Expr] =
//    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpr
//
//  lazy val jsonExpression: P[Expr] =
//    (arrayExpression | objectExpression) ^^ JSONExpr

  lazy val application: P[Expr] =
    identifier ~ ("(" ~> expressions <~ ")") ^^ { case f ~ as => ApplyExpr(f, as) }

  lazy val column: P[ColumnExpr] = positioned(
    identifier ~ opt("." ~> identifier) ^^ {
      case c ~ None                          => ColumnExpr(c)
      case (tid @ Ident(t)) ~ Some(Ident(c)) => ColumnExpr(Ident(s"$t.$c").setPos(tid.pos))
    }
  )

  lazy val primary: P[Expr] = positioned(
    numericLit ^^ (n => NumberExpr(n.toInt)) |
      application |
      column |
//      jsonExpression |
//      caseExpression |
//      "-" ~> primary ^^ (e => UnaryExpr("-", e)) |
//      "(" ~> query <~ ")" ^^ SubqueryExpr.apply |
      "(" ~> expression <~ ")"
  )

//  lazy val caseExpression: P[CaseExpr] =
//    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ { case ws ~ e => CaseExpr(ws, e) }
//
//  lazy val when: P[OQLWhen] =
//    kw("WHEN") ~ booleanExpression ~ kw("THEN") ~ expression ^^ { case _ ~ l ~ _ ~ e => OQLWhen(l, e) }

//  lazy val float: P[Number] = """[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?""".r ^^ (_.toDouble.asInstanceOf[Number])
//
//  lazy val integer: P[Number] = "[0-9]+".r ^^ (_.toInt.asInstanceOf[Number])
//
//  lazy val singleQuoteString: P[String] =
//    """'(?:[^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r ^^ (s => s.substring(1, s.length - 1))
//
//  lazy val doubleQuoteString: P[String] =
//    """"(?:[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ (s => s.substring(1, s.length - 1))
//
//  lazy val string: P[String] = singleQuoteString | doubleQuoteString

  def parseQuery(input: String): SQLSelectExpr =
    val tokens = new PackratReader(new lexical.Scanner(input))

    phrase(query)(tokens) match
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)

//  def parseBooleanExpression(input: String): Expr =
//    parseAll(phrase(booleanExpression), new PackratReader(new CharSequenceReader(input))) match {
//      case Success(result, _)   => result
//      case Failure(error, rest) => problem(rest.pos, error)
//      case Error(error, rest)   => problem(rest.pos, error)
//    }
