package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{Position, Positional}

object SQLParser extends StandardTokenParsers with PackratParsers:

  class SQLLexer extends StdLexical:
    delimiters ++= Seq(
      "+",
      "-",
      "*",
      "/",
      "%",
      "(",
      ")",
      ".",
      "||",
      "<=",
      ">=",
      "<",
      ">",
      "=",
      "!=",
      ",",
      "&",
      "|",
      "^",
      "@",
      "{",
      "}",
      ":",
      "[",
      "]"
    )
    reserved ++= Seq(
      "ADD",
      "ALL",
      "ALTER",
      "AND",
      "ANY",
      "AS",
      "ASC",
      "BETWEEN",
      "BOOLEAN",
      "BY",
      "CASE",
      "CHECK",
      "COLUMN",
      "CONSTRAINT",
      "CREATE",
      "DROP",
      "DATABASE",
      "DEFAULT",
      "DELETE",
      "DESC",
      "DISTINCT",
      "ELSE",
      "END",
      "EXEC",
      "EXISTS",
      "FALSE",
      "FIRST",
      "FLOAT",
      "FOREIGN",
      "FROM",
      "GROUP",
      "HAVING",
      "ILIKE",
      "IN",
      "INDEX",
      "INNER",
      "INSERT",
      "INT",
      "INTO",
      "INTEGER",
      "IS",
      "JOIN",
      "JSON",
      "KEY",
      "LAST",
      "LEFT",
      "LIKE",
      "LIMIT",
      "NOT",
      "NULL",
      "NULLS",
      "OFFSET",
      "ON",
      "OR",
      "ORDER",
      "PRIMARY",
      "PROCEDURE",
      "REFERENCES",
      "SELECT",
      "SET",
      "SOME",
      "TABLE",
      "TEXT",
      "TIMESTAMP",
      "THEN",
      "TRUE",
      "UPDATE",
      "UNION",
      "UNIQUE",
      "VALUES",
      "WHEN",
      "WHERE"
    )

    case class DecimalLit(chars: String) extends Token {
      override def toString: String = chars
    }

    override def token: Parser[Token] = quotedToken | stringToken | decimalToken | super.token

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

    private def quotedToken: Parser[Token] =
      '"' ~> rep(guard(not('"')) ~> elem("", _ => true)) <~ '"' ^^ { l => Identifier(l mkString) }

    private def stringToken: Parser[Token] =
      '\'' ~> rep(guard(not('\'')) ~> (('\\' ~ '\'' ^^^ "\\'") | elem("", _ => true))) <~ '\'' ^^ (l =>
        StringLit(unescape(l mkString))
      )

  override val lexical: SQLLexer = new SQLLexer

  import lexical.DecimalLit

  def decimalLit: Parser[String] =
    elem("decimal", _.isInstanceOf[DecimalLit]) ^^ (_.asInstanceOf[DecimalLit].chars)

  type P[+T] = PackratParser[T]

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val query: P[SQLSelectExpr] =
    "SELECT" ~ expressions ~ fromClause ~ whereClause ~ groupByClause ~ orderByClause ~ offsetClause ~ limitClause ^^ {
      case _ ~ p ~ f ~ w ~ g ~ o ~ of ~ l =>
        SQLSelectExpr(p to ArraySeq, f, w, g, o, of, l)
    }

  lazy val fromClause: P[Option[Seq[Expr]]] = opt("FROM" ~> rep1sep(sources, ","))

  lazy val whereClause: P[Option[Expr]] = opt("WHERE" ~> booleanExpression)

  lazy val groupByClause: P[Option[Seq[Expr]]] = opt("GROUP" ~> "BY" ~> rep1sep(expression, ","))

  lazy val orderByClause: P[Option[Seq[OrderBy]]] = opt("ORDER" ~> "BY" ~> rep1sep(orderBy, ","))

  lazy val count: P[Count] = pos ~ integer ^^ { case p ~ c => Count(p, c) }

  lazy val offsetClause: P[Option[Count]] = opt("OFFSET" ~> count)

  lazy val limitClause: P[Option[Count]] = opt("LIMIT" ~> count)

  lazy val orderBy: P[OrderBy] = expression ~ opt("ASC" | "DESC") ~ opt("NULLS" ~> ("FIRST" | "LAST")) ^^ {
    case e ~ (None | Some("ASC")) ~ (None | Some("FIRST")) => OrderBy(e, true, true)
    case e ~ _ ~ (None | Some("FIRST"))                    => OrderBy(e, false, true)
    case e ~ (None | Some("ASC")) ~ _                      => OrderBy(e, true, false)
    case e ~ _ ~ _                                         => OrderBy(e, false, false)
  }

  lazy val sources: P[Expr] =
    sources ~ opt("INNER" | ("LEFT" ~ opt("OUTER"))) ~ "JOIN" ~ source ~ "ON" ~ booleanExpression ^^ {
      case l ~ (None | Some("INNER")) ~ _ ~ r ~ _ ~ c => InnerJoinOperator(l, r, c)
      case l ~ /*Some("LEFT" ~ _)*/ _ ~ _ ~ r ~ _ ~ c => LeftJoinOperator(l, r, c)
    } | source

  lazy val source: P[Expr] =
    (table | ("(" ~> query <~ ")")) ~ opt(opt("AS") ~> identifier) ^^ {
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

  lazy val booleanExpression: P[Expr] = orExpression

  lazy val orExpression: P[Expr] = positioned(
    orExpression ~ "OR" ~ andExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "OR", r) } |
      andExpression
  )

  lazy val andExpression: P[Expr] = positioned(
    andExpression ~ "AND" ~ notExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "AND", r) } |
      notExpression
  )

  lazy val notExpression: P[Expr] = positioned(
    "NOT" ~> booleanPrimary ^^ (e => UnaryExpr("NOT", e)) |
      booleanPrimary
  )

  lazy val booleanPrimary: P[Expr] = positioned(
    "EXISTS" ~> "(" ~> query <~ ")" ^^ ExistsExpr.apply |
      expression ~ comparison ~ expression ^^ { case l ~ c ~ r => BinaryExpr(l, c, r) } |
      expression ~ ("NOT" ~ "BETWEEN" ^^^ "NOT BETWEEN" | "BETWEEN") ~ expression ~ "AND" ~ expression ^^ {
        case e ~ b ~ l ~ _ ~ u =>
          BetweenExpr(e, b, l, u)
      } |
      expression ~ isNull ^^ { case e ~ n => UnaryExpr(n, e) } |
      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => InSeqExpr(e, i, es) } |
      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => InQueryExpr(e, i, q) } |
      booleanLiteral |
      "(" ~> booleanExpression <~ ")"
  )

  lazy val isNull: P[String] =
    "IS" ~ "NULL" ^^^ "IS NULL" | "IS" ~ "NOT" ~ "NULL" ^^^ "IS NOT NULL"

  lazy val in: P[String] = "NOT" ~ "IN" ^^^ "NOT IN" | "IN"

  lazy val comparison: P[String] =
    "<=" | ">=" | "<" | ">" | "=" | "!=" | "LIKE" | "ILIKE" | ("NOT" ~ "LIKE" ^^^ "NOT LIKE" | "NOT" ~ "ILIKE" ^^^ "NOT ILIKE")

  lazy val booleanLiteral: P[Expr] = positioned(
    ("TRUE" | "FALSE") ^^ (s => BooleanExpr(s.equalsIgnoreCase("TRUE")))
  )

  lazy val expression: P[Expr] = concatenation

  lazy val concatenation: P[Expr] = positioned(
    concatenation ~ "||" ~ additive ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      additive
  )

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

  lazy val pair: P[(Ident, Expr)] =
    identifier ~ ":" ~ (arrayExpression | objectExpression | literal) ^^ { case k ~ _ ~ v =>
      k -> v
    }

  lazy val arrayExpression: P[Expr] = positioned(
    "[" ~> repsep(arrayExpression | objectExpression | literal, ",") <~ "]" ^^ ArrayExpr.apply
  )

  lazy val objectExpression: P[Expr] = positioned(
    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpr.apply
  )

  lazy val jsonLiteral: P[Expr] = arrayExpression | objectExpression

  lazy val application: P[Expr] = positioned(
    identifier ~ ("(" ~> expressions <~ ")") ^^ { case f ~ as => ApplyExpr(f, as) }
  )

  lazy val column: P[ColumnExpr] = positioned(
    identifier ~ opt("." ~> identifier) ^^ {
      case c ~ None                          => ColumnExpr(c)
      case (tid @ Ident(t)) ~ Some(Ident(c)) => ColumnExpr(Ident(s"$t.$c").setPos(tid.pos))
    }
  )

  lazy val integer: P[Int] = numericLit ^^ (_.toInt)

  lazy val decimal: P[Double] = decimalLit ^^ (_.toDouble)

  lazy val primary: P[Expr] = positioned(
    decimal ^^ (n => NumberExpr(n)) |
      integer ^^ (n => NumberExpr(n)) |
      stringLit ^^ StringExpr.apply |
      application |
      column |
      booleanLiteral |
      jsonLiteral |
      caseExpression |
      "-" ~> primary ^^ (e => UnaryExpr("-", e)) |
//      "(" ~> query <~ ")" ^^ SubqueryExpr.apply |
      "TABLE" ~> "(" ~> query <~ ")" ^^ TableConstructorExpr.apply |
      "(" ~> expression <~ ")"
  )

  lazy val literal: P[Expr] = positioned(
    booleanLiteral |
      jsonLiteral |
      decimal ^^ (n => NumberExpr(n)) |
      integer ^^ (n => NumberExpr(n)) |
      stringLit ^^ StringExpr.apply |
      "-" ~> primary ^^ (e => UnaryExpr("-", e))
  )

  lazy val caseExpression: P[CaseExpr] =
    "CASE" ~> rep1(when) ~ opt("ELSE" ~> expression) <~ "END" ^^ { case ws ~ e => CaseExpr(ws, e) }

  lazy val when: P[When] =
    "WHEN" ~> booleanExpression ~ "THEN" ~ expression ^^ { case l ~ _ ~ e => When(l, e) }

  lazy val row: P[Seq[Expr]] = "(" ~> rep1sep(literal, ",") <~ ")"

  lazy val set: P[UpdateSet] = identifier ~ "=" ~ expression ^^ { case c ~ _ ~ v => UpdateSet(c, v) }

  lazy val insert: P[Command] =
    "INSERT" ~> "INTO" ~> identifier ~ ("(" ~> rep1sep(identifier, ",") <~ ")") ~ "VALUES" ~ rep1sep(row, ",") ^^ {
      case t ~ cs ~ _ ~ rs => InsertCommand(t, cs, rs)
    }

  lazy val create: P[Command] =
    "CREATE" ~> "TABLE" ~> identifier ~ ("(" ~> rep1sep(columnDesc, ",") <~ ")") ^^ { case t ~ cs =>
      CreateTableCommand(t, cs)
    }

  lazy val update: P[Command] =
    "UPDATE" ~> identifier ~ "SET" ~ rep1sep(set, ",") ~ opt("WHERE" ~> booleanExpression) ^^ { case t ~ _ ~ ss ~ c =>
      UpdateCommand(t, ss, c)
    }

  lazy val delete: P[Command] =
    "DELETE" ~> "FROM" ~> identifier ~ opt("WHERE" ~> booleanExpression) ^^ { case t ~ c =>
      DeleteCommand(t, c)
    }

  lazy val typ: P[String] = "BOOLEAN" | "INT" | "INTEGER" | "DOUBLE" | "JSON" | "TIMESTAMP" | "TEXT"

  lazy val columnDesc: P[ColumnDesc] = identifier ~ typ ~ opt("PRIMARY" ~ "KEY") ~ opt("NOT" ~ "NULL") ^^ {
    case c ~ t ~ p ~ n => ColumnDesc(c, t, p.isDefined, n.isDefined)
  }

  lazy val command: P[Command] =
    query ^^ QueryCommand.apply |
      insert |
      create |
      update

  def parseQuery(input: String): SQLSelectExpr =
    val tokens = new PackratReader(new lexical.Scanner(input))

    phrase(query)(tokens) match
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)

  def parseCommand(input: String): Command =
    val tokens = new PackratReader(new lexical.Scanner(input))

    phrase(command)(tokens) match
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)
