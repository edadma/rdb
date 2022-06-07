package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps
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
      "]",
      ";",
    )
    reserved ++= Seq(
      "ADD",
      "add",
      "ALL",
      "all",
      "ALTER",
      "alter",
      "AND",
      "and",
      "ANY",
      "any",
      "AS",
      "as",
      "ASC",
      "asc",
      "AUTO",
      "auto",
      "BETWEEN",
      "between",
      "BIGINT",
      "bigint",
      "BOOLEAN",
      "boolean",
      "BY",
      "by",
      "CASE",
      "case",
      "CHECK",
      "check",
      "COLUMN",
      "column",
      "CONSTRAINT",
      "constraint",
      "CREATE",
      "create",
      "CURRENT_TIMESTAMP",
      "current_timestamp",
      "DATABASE",
      "database",
      "DEFAULT",
      "default",
      "DELETE",
      "delete",
      "DESC",
      "desc",
      "DISTINCT",
      "distinct",
      "DOUBLE",
      "double",
      "DROP",
      "drop",
      "ELSE",
      "else",
      "END",
      "end",
      "ENUM",
      "enum",
      "EXEC",
      "exec",
      "EXISTS",
      "exists",
      "FALSE",
      "false",
      "FIRST",
      "first",
      "FLOAT",
      "float",
      "FOREIGN",
      "foreign",
      "FROM",
      "from",
      "GROUP",
      "group",
      "HAVING",
      "having",
      "ILIKE",
      "ilike",
      "IN",
      "in",
      "INDEX",
      "index",
      "INNER",
      "inner",
      "INSERT",
      "insert",
      "INT",
      "int",
      "INTEGER",
      "integer",
      "INTO",
      "into",
      "IS",
      "is",
      "JOIN",
      "join",
      "JSON",
      "json",
      "KEY",
      "key",
      "LAST",
      "last",
      "LEFT",
      "left",
      "LIKE",
      "like",
      "LIMIT",
      "limit",
      "NOT",
      "not",
      "NULL",
      "null",
      "NULLS",
      "nulls",
      "NUMERIC",
      "numeric",
      "OFFSET",
      "offset",
      "ON",
      "on",
      "OR",
      "or",
      "ORDER",
      "order",
      "PRECISION",
      "precision",
      "PRIMARY",
      "primary",
      "PROCEDURE",
      "procedure",
      "REFERENCES",
      "references",
      "RETURNING",
      "returning",
      "SELECT",
      "select",
      "SET",
      "set",
      "SOME",
      "some",
      "TABLE",
      "table",
      "TEXT",
      "text",
      "THEN",
      "then",
      "TIME",
      "time",
      "TIMESTAMP",
      "timestamp",
      "TRUE",
      "true",
      "TYPE",
      "type",
      "UNION",
      "union",
      "UNIQUE",
      "unique",
      "UPDATE",
      "update",
      "UUID",
      "uuid",
      "VALUES",
      "values",
      "WHEN",
      "when",
      "WHERE",
      "where",
      "WITHOUT",
      "without",
      "ZONE",
      "zone",
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
      opt('E') ~> '\'' ~> rep(guard(not('\'')) ~> (('\\' ~ '\'' ^^^ "\\'") | elem("", _ => true))) <~ '\'' ^^ (l =>
        StringLit(unescape(l mkString)),
      )

  override val lexical: SQLLexer = new SQLLexer

  import lexical.DecimalLit

  def decimalLit: Parser[String] =
    elem("decimal", _.isInstanceOf[DecimalLit]) ^^ (_.asInstanceOf[DecimalLit].chars)

  type P[+T] = PackratParser[T]

  def kw(s: String): P[String] = s.toLowerCase | s.toUpperCase

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val query: P[SQLSelectExpr] =
    kw(
      "SELECT",
    ) ~ expressions ~ fromClause ~ whereClause ~ groupByClause ~ orderByClause ~ limitClause ~ offsetClause ^^ {
      case _ ~ p ~ f ~ w ~ g ~ o ~ of ~ l =>
        SQLSelectExpr(p to ArraySeq, f, w, g, o, l, of)
    }

  lazy val fromClause: P[Option[Seq[Expr]]] = opt(kw("FROM") ~> rep1sep(sources, ","))

  lazy val whereClause: P[Option[Expr]] = opt(kw("WHERE") ~> booleanExpression)

  lazy val groupByClause: P[Option[Seq[Expr]]] = opt(kw("GROUP") ~> kw("BY") ~> rep1sep(expression, ","))

  lazy val orderByClause: P[Option[Seq[OrderBy]]] = opt(kw("ORDER") ~> kw("BY") ~> rep1sep(orderBy, ","))

  lazy val count: P[Count] = pos ~ integer ^^ { case p ~ c => Count(p, c) }

  lazy val offsetClause: P[Option[Count]] = opt(kw("OFFSET") ~> count)

  lazy val limitClause: P[Option[Count]] = opt(kw("LIMIT") ~> count)

  lazy val orderBy: P[OrderBy] =
    expression ~ opt(kw("ASC") | kw("DESC")) ~ opt(kw("NULLS") ~> (kw("FIRST") | kw("LAST"))) ^^ {
      case e ~ (None | Some("ASC")) ~ (None | Some("FIRST")) => OrderBy(e, true, true)
      case e ~ _ ~ (None | Some("FIRST"))                    => OrderBy(e, false, true)
      case e ~ (None | Some("ASC")) ~ _                      => OrderBy(e, true, false)
      case e ~ _ ~ _                                         => OrderBy(e, false, false)
    }

  lazy val sources: P[Expr] =
    sources ~ opt(kw("INNER") | (kw("LEFT") ~ opt(kw("OUTER")))) ~ kw("JOIN") ~ source ~ kw(
      "ON",
    ) ~ booleanExpression ^^ {
      case l ~ (None | Some("INNER")) ~ _ ~ r ~ _ ~ c => InnerJoinOperator(l, r, c)
      case l ~ _ ~ _ ~ r ~ _ ~ c                      => LeftJoinOperator(l, r, c)
    } | source

  lazy val source: P[Expr] =
    (table | ("(" ~> query <~ ")")) ~ opt(opt(kw("AS")) ~> identifier) ^^ {
      case s ~ None    => s
      case s ~ Some(a) => AliasOperator(s, a)
    }

  lazy val table: P[Expr] = positioned(
    identifier ^^ TableOperator.apply,
  )

  lazy val star: P[Expr] = positioned(
    "*" ^^^ StarExpr(),
  )

  lazy val identifier: P[Ident] = positioned(
    ident ^^ Ident.apply,
  )

  lazy val expressions: P[Seq[Expr]] = rep1sep(expression | star, ",")

  lazy val booleanExpression: P[Expr] = orExpression

  lazy val orExpression: P[Expr] = positioned(
    orExpression ~ kw("OR") ~ andExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "OR", r) } |
      andExpression,
  )

  lazy val andExpression: P[Expr] = positioned(
    andExpression ~ kw("AND") ~ notExpression ^^ { case l ~ _ ~ r => BinaryExpr(l, "AND", r) } |
      notExpression,
  )

  lazy val notExpression: P[Expr] = positioned(
    kw("NOT") ~> booleanPrimary ^^ (e => UnaryExpr("NOT", e)) |
      booleanPrimary,
  )

  lazy val booleanPrimary: P[Expr] = positioned(
    kw("EXISTS") ~> "(" ~> query <~ ")" ^^ ExistsExpr.apply |
      expression ~ comparison ~ expression ^^ { case l ~ c ~ r => BinaryExpr(l, c, r) } |
      expression ~ (kw("NOT") ~ kw("BETWEEN") ^^^ "NOT BETWEEN" | kw("BETWEEN")) ~ expression ~ kw(
        "AND",
      ) ~ expression ^^ { case e ~ b ~ l ~ _ ~ u =>
        BetweenExpr(e, b, l, u)
      } |
      expression ~ isNull ^^ { case e ~ n => UnaryExpr(n, e) } |
      expression ~ in ~ ("(" ~> expressions <~ ")") ^^ { case e ~ i ~ es => InSeqExpr(e, i, es) } |
      expression ~ in ~ ("(" ~> query <~ ")") ^^ { case e ~ i ~ q => InQueryExpr(e, i, q) } |
      booleanLiteral |
      kw("NULL") ^^^ NullExpr() |
      "(" ~> booleanExpression <~ ")",
  )

  lazy val isNull: P[String] =
    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"

  lazy val in: P[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")

  lazy val comparison: P[String] =
    "<=" | ">=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE" | kw(
      "NOT",
    ) ~ kw("ILIKE") ^^^ "NOT ILIKE")

  lazy val booleanLiteral: P[Expr] = positioned(
    (kw("TRUE") | kw("FALSE")) ^^ (s => BooleanExpr(s.equalsIgnoreCase("TRUE"))),
  )

  lazy val expression: P[Expr] = concatenation

  lazy val concatenation: P[Expr] = positioned(
    concatenation ~ "||" ~ additive ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      additive,
  )

  lazy val additive: P[Expr] = positioned(
    additive ~ ("+" | "-") ~ multiplicative ^^ { case l ~ o ~ r =>
      BinaryExpr(l, o, r)
    } |
      multiplicative,
  )

  lazy val multiplicative: P[Expr] = positioned(
    positioned(
      multiplicative ~ ("*" | "/") ~ primary ^^ { case l ~ o ~ r =>
        BinaryExpr(l, o, r)
      } |
        primary,
    ),
  )

  lazy val pair: P[(Ident, Expr)] =
    identifier ~ ":" ~ (arrayExpression | objectExpression | literal) ^^ { case k ~ _ ~ v =>
      k -> v
    }

  lazy val arrayExpression: P[Expr] = positioned(
    "[" ~> repsep(arrayExpression | objectExpression | literal, ",") <~ "]" ^^ ArrayExpr.apply,
  )

  lazy val objectExpression: P[Expr] = positioned(
    "{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpr.apply,
  )

  lazy val jsonLiteral: P[Expr] = arrayExpression | objectExpression

  lazy val application: P[Expr] = positioned(
    identifier ~ ("(" ~> expressions <~ ")") ^^ { case f ~ as => ApplyExpr(f, as) },
  )

  lazy val column: P[ColumnExpr] = positioned(
    identifier ~ opt("." ~> identifier) ^^ {
      case c ~ None                          => ColumnExpr(c)
      case (tid @ Ident(t)) ~ Some(Ident(c)) => ColumnExpr(Ident(s"$t.$c").setPos(tid.pos))
    },
  )

  lazy val variable: P[VariableExpr] = positioned(
    pos ~ "CURRENT_TIMESTAMP" ^^ { case p ~ v => VariableExpr(Ident(v).setPos(p)) },
  )

  lazy val integer: P[Int] = numericLit ^^ (_.toInt)

  lazy val decimal: P[Double] = decimalLit ^^ (_.toDouble)

  lazy val primary: P[Expr] = positioned(
    decimal ^^ (n => NumberExpr(n)) |
      integer ^^ (n => NumberExpr(n)) |
      stringLit ^^ StringExpr.apply |
      kw("NULL") ^^^ NullExpr() |
      application |
      column |
      variable |
      booleanLiteral |
      jsonLiteral |
      caseExpression |
      "-" ~> primary ^^ (e => UnaryExpr("-", e)) |
      kw("TABLE") ~> "(" ~> query <~ ")" ^^ TableConstructorExpr.apply |
      "(" ~> query <~ ")" ^^ SubqueryExpr.apply |
      "(" ~> expression <~ ")",
  )

  lazy val literal: P[Expr] = positioned(
    booleanLiteral |
      jsonLiteral |
      decimal ^^ (n => NumberExpr(n)) |
      integer ^^ (n => NumberExpr(n)) |
      stringLit ^^ StringExpr.apply |
      kw("NULL") ^^^ NullExpr() |
      "-" ~> primary ^^ (e => UnaryExpr("-", e)),
  )

  lazy val caseExpression: P[CaseExpr] =
    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ { case ws ~ e => CaseExpr(ws, e) }

  lazy val when: P[When] =
    kw("WHEN") ~> booleanExpression ~ kw("THEN") ~ expression ^^ { case l ~ _ ~ e => When(l, e) }

  lazy val row: P[Seq[Expr]] = "(" ~> rep1sep(literal, ",") <~ ")"

  lazy val set: P[UpdateSet] = identifier ~ "=" ~ expression ^^ { case c ~ _ ~ v => UpdateSet(c, v) }

  lazy val insert: P[Command] =
    kw("INSERT") ~> kw("INTO") ~> identifier ~ ("(" ~> rep1sep(identifier, ",") <~ ")") ~ kw("VALUES") ~ rep1sep(
      row,
      ",",
    ) ~ opt(
      kw("RETURNING") ~> identifier,
    ) ^^ { case t ~ cs ~ _ ~ rs ~ ret =>
      InsertCommand(t, cs, rs, ret)
    }

  lazy val createTable: P[Command] =
    kw("CREATE") ~> kw("TABLE") ~> identifier ~ ("(" ~> rep1sep(columnDesc, ",") <~ ")") ^^ { case t ~ cs =>
      CreateTableCommand(t, cs)
    }

  lazy val createEnum: P[Seq[String]] = kw("ENUM") ~> ("(" ~> rep1sep(stringLit, ",") <~ ")")

  lazy val createType: P[Command] =
    kw("CREATE") ~> kw("TYPE") ~> identifier ~ (kw("AS") ~> createEnum) ^^ { case t ~ ls =>
      CreateEnumCommand(t, ls)
    }

  lazy val update: P[Command] =
    kw("UPDATE") ~> identifier ~ kw("SET") ~ rep1sep(set, ",") ~ opt(kw("WHERE") ~> booleanExpression) ^^ {
      case t ~ _ ~ ss ~ c =>
        UpdateCommand(t, ss, c)
    }

  lazy val delete: P[Command] =
    kw("DELETE") ~> kw("FROM") ~> identifier ~ opt(kw("WHERE") ~> booleanExpression) ^^ { case t ~ c =>
      DeleteCommand(t, c)
    }

  lazy val typ: P[Either[Type, Ident]] =
    kw("BOOLEAN") ^^^ Left(BooleanType)
      | (kw("INT") | kw("INTEGER")) ^^^ Left(IntegerType)
      | kw("BIGINT") ^^^ Left(BigintType)
      | kw("DOUBLE") ~ opt(kw("PRECISION")) ^^^ Left(DoubleType)
      | kw("NUMERIC") ~> ("(" ~> integer ~ ("," ~> integer) <~ ")") ^^ { case p ~ s => Left(NumericType(p, s)) }
      | kw("JSON") ^^^ Left(JSONType)
      | kw("TIMESTAMP") ~ opt(kw("WITHOUT") ~ kw("TIME") ~ kw("ZONE")) ^^^ Left(TimestampType)
      | kw("TEXT") ^^^ Left(TextType)
      | kw("UUID") ^^^ Left(UUIDType)
      | identifier ^^ Right.apply

  lazy val columnDesc: P[ColumnDesc] =
    identifier ~ typ ~ opt(kw("AUTO")) ~ opt(kw("NOT") ~ kw("NULL")) ~ opt(kw("PRIMARY") ~ kw("KEY")) ^^ {
      case c ~ t ~ a ~ n ~ p =>
        ColumnDesc(c, t, a.isDefined, n.isDefined, p.isDefined)
    }

  lazy val alterTable: P[Command] =
    kw("ALTER") ~> kw("TABLE") ~> identifier ~ tableAlteration ^^ { case t ~ a =>
      AlterTableCommand(t, a)
    }

  lazy val tableAlteration: P[TableAlteration] =
    kw("ADD") ~> kw("FOREIGN") ~> kw("KEY") ~> ("(" ~> identifier <~ ")") ~ (kw("REFERENCES") ~> identifier) ^^ {
      case fk ~ ref =>
        AddForeignKeyTableAlteration(fk, ref)
    }

  lazy val command: P[Command] =
    query ^^ QueryCommand.apply |
      insert |
      createTable |
      createType |
      update |
      delete |
      alterTable

  lazy val commands: P[Seq[Command]] = rep1sep(command, ";") <~ opt(";")

  def parse[T](input: String, parser: P[T]): T =
    val tokens = new PackratReader(new lexical.Scanner(input))

    phrase(parser)(tokens) match
      case Success(result, _)   => result
      case Failure(error, rest) => problem(rest.pos, error)
      case Error(error, rest)   => problem(rest.pos, error)

  def parseQuery(input: String): SQLSelectExpr = parse(input, query)

  def parseCommand(input: String): Command = parse(input, command)

  def parseCommands(input: String): Seq[Command] = parse(input, commands)
