package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader.EofCh
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
      "::",
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
      "ARRAY",
      "array",
      "AS",
      "as",
      "ASC",
      "asc",
      "BETWEEN",
      "between",
      "BIGINT",
      "bigint",
      "BOOLEAN",
      "boolean",
      "BY",
      "by",
      "CASCADE",
      "cascade",
      "BYTEA",
      "bytea",
      "CASE",
      "case",
      "CHAR",
      "char",
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
      "DATE",
      "date",
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
      "EXTRACT",
      "extract",
      "FALSE",
      "false",
      "FIRST",
      "first",
      "DECIMAL",
      "decimal",
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
      "IF",
      "if",
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
      "INTERVAL",
      "interval",
      "INTO",
      "into",
      "IS",
      "is",
      "JOIN",
      "join",
      "JSONB",
      "jsonb",
      "JSON",
      "json",
      "KEY",
      "key",
      "LAST",
      "last",
      "CROSS",
      "cross",
      "FULL",
      "full",
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
      "OUTER",
      "outer",
      "PROCEDURE",
      "procedure",
      "SMALLINT",
      "smallint",
      "SMALLSERIAL",
      "smallserial",
      "SERIAL",
      "serial",
      "BIGSERIAL",
      "bigserial",
      "REAL",
      "real",
      "REFERENCES",
      "references",
      "RENAME",
      "rename", 
      "RESTRICT", 
      "restrict",
      "RETURNING",
      "returning",
      "RIGHT",
      "right",
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
      "TO",
      "to",
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
      "INTERSECT",
      "intersect",
      "EXCEPT",
      "except",
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
      "WITH",
      "with",
      "WITHOUT",
      "without",
      "ZONE",
      "zone",
    )

    case class DecimalLit(chars: String) extends Token {
      override def toString: String = chars
    }

    override def token: Parser[Token] = quotedToken | stringToken | decimalToken | super.token

    // Add support for SQL comments
    override def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
        | '/' ~ '*' ~ comment                     // Keep /* */ block comments
        | '-' ~ '-' ~ rep(chrExcept(EofCh, '\n')) // Add -- line comments for SQL
        | '/' ~ '*' ~ rep(elem("", _ => true)) ~> err("unclosed comment"),
    )

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
        StringLit(unescape(l mkString))
      )

  override val lexical: SQLLexer = new SQLLexer

  import lexical.DecimalLit

  def decimalLit: Parser[String] =
    elem("decimal", _.isInstanceOf[DecimalLit]) ^^ (_.asInstanceOf[DecimalLit].chars)

  type P[+T] = PackratParser[T]

  def kw(s: String): P[String] = s.toLowerCase | s.toUpperCase

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val selectCore: P[Expr] =
    kw(
      "SELECT",
    ) ~ opt(kw("DISTINCT")) ~ selectExpressions ~ fromClause ~ whereClause ~ groupByClause ~ havingClause ^^ {
      case _ ~ d ~ p ~ f ~ w ~ g ~ h =>
        SQLSelectExpr(p to ArraySeq, f, w, g, h, None, None, None, distinct = d.isDefined)
    } | "(" ~> compoundSelect <~ ")"

  lazy val intersectSelect: P[Expr] =
    intersectSelect ~ kw("INTERSECT") ~ selectCore ^^ { case l ~ _ ~ r =>
      SetOperationExpr("INTERSECT", l, r)
    } | selectCore

  lazy val compoundSelect: P[Expr] =
    compoundSelect ~ kw("UNION") ~ kw("ALL") ~ intersectSelect ^^ { case l ~ _ ~ _ ~ r =>
      SetOperationExpr("UNION ALL", l, r)
    } |
      compoundSelect ~ kw("UNION") ~ intersectSelect ^^ { case l ~ _ ~ r =>
        SetOperationExpr("UNION", l, r)
      } |
      compoundSelect ~ kw("EXCEPT") ~ intersectSelect ^^ { case l ~ _ ~ r =>
        SetOperationExpr("EXCEPT", l, r)
      } |
      intersectSelect

  lazy val query: P[Expr] =
    compoundSelect ~ orderByClause ~ limitClause ~ offsetClause ^^ {
      case (s: SQLSelectExpr) ~ o ~ l ~ of            => s.copy(orderBy = o, limit = l, offset = of)
      case s ~ None ~ None ~ None                     => s
      case s ~ o ~ l ~ of                             => CompoundQueryExpr(s, o, of, l)
    }

  lazy val fromClause: P[Option[Seq[Expr]]] = opt(kw("FROM") ~> rep1sep(sources, ","))

  lazy val whereClause: P[Option[Expr]] = opt(kw("WHERE") ~> booleanExpression)

  lazy val groupByClause: P[Option[Seq[Expr]]] = opt(kw("GROUP") ~> kw("BY") ~> rep1sep(expression, ","))

  lazy val havingClause: P[Option[Expr]] = opt(kw("HAVING") ~> booleanExpression)

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

  lazy val joinType: P[String] =
    kw("INNER") ^^^ "INNER" |
      kw("LEFT") ~ opt(kw("OUTER")) ^^^ "LEFT" |
      kw("RIGHT") ~ opt(kw("OUTER")) ^^^ "RIGHT" |
      kw("FULL") ~ opt(kw("OUTER")) ^^^ "FULL"

  lazy val sources: P[Expr] =
    sources ~ kw("CROSS") ~ kw("JOIN") ~ source ^^ {
      case l ~ _ ~ _ ~ r => CrossOperator(l, r)
    } |
      sources ~ opt(joinType) ~ kw("JOIN") ~ source ~ kw("ON") ~ booleanExpression ^^ {
        case l ~ (None | Some("INNER")) ~ _ ~ r ~ _ ~ c => InnerJoinOperator(l, r, c)
        case l ~ Some("LEFT") ~ _ ~ r ~ _ ~ c           => LeftJoinOperator(l, r, c)
        case l ~ Some("RIGHT") ~ _ ~ r ~ _ ~ c          => RightJoinOperator(l, r, c)
        case l ~ Some("FULL") ~ _ ~ r ~ _ ~ c           => FullJoinOperator(l, r, c)
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

  lazy val selectExpression: P[Expr] =
    (expression | star) ~ opt(opt(kw("AS")) ~> identifier) ^^ {
      case e ~ None    => e
      case e ~ Some(a) => AliasExpr(e, a)
    }

  lazy val selectExpressions: P[Seq[Expr]] = rep1sep(selectExpression, ",")

  lazy val expressions: P[Seq[Expr]] = rep1sep(expression, ",")

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
      multiplicative ~ ("*" | "/") ~ castExpression ^^ { case l ~ o ~ r =>
        BinaryExpr(l, o, r)
      } |
        castExpression,
    ),
  )

  lazy val extractField: P[String] =
    ident ^^ (_.toLowerCase) |
      kw("TIMESTAMP") ^^^ "timestamp" |
      kw("TIME") ^^^ "time" |
      kw("DATE") ^^^ "date"

  lazy val castType: P[Type] = typ ^? (
    { case Left(t) => t },
    _ => "cannot cast to custom type",
  )

  lazy val castExpression: P[Expr] = positioned(
    primary ~ "::" ~ castType ^^ { case e ~ _ ~ t => CastExpr(e, t) }
      | primary,
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
    identifier ~ ("(" ~> repsep(expression | star, ",") <~ ")") ^^ { case f ~ as => ApplyExpr(f, as) },
  )

  lazy val column: P[ColumnExpr] = positioned(
    identifier ~ opt("." ~> identifier) ^^ {
      case c ~ None    => ColumnExpr(None, c)
      case t ~ Some(c) => ColumnExpr(Some(t), c)
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
      kw("ARRAY") ~> "[" ~> repsep(expression, ",") <~ "]" ^^ ArrayExpr.apply |
      kw("EXTRACT") ~> "(" ~> extractField ~ kw("FROM") ~ expression <~ ")" ^^ { case field ~ _ ~ source =>
        ApplyExpr(Ident("date_part"), Seq(StringExpr(field), source))
      } |
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
    simpleCaseExpression | searchedCaseExpression

  lazy val simpleCaseExpression: P[CaseExpr] =
    kw("CASE") ~> expression ~ rep1(simpleWhen) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ {
      case expr ~ whens ~ els =>
        // Convert to searched CASE internally
        val searchedWhens = whens.map { case (value, result) =>
          When(BinaryExpr(expr, "=", value), result)
        }
        CaseExpr(searchedWhens, els)
    }

  lazy val searchedCaseExpression: P[CaseExpr] =
    kw("CASE") ~> rep1(when) ~ opt(kw("ELSE") ~> expression) <~ kw("END") ^^ {
      case ws ~ e => CaseExpr(ws, e)
    }

  lazy val simpleWhen: P[(Expr, Expr)] =
    kw("WHEN") ~> expression ~ kw("THEN") ~ expression ^^ { case v ~ _ ~ r => (v, r) }

  lazy val when: P[When] =
    kw("WHEN") ~> booleanExpression ~ kw("THEN") ~ expression ^^ { case l ~ _ ~ e => When(l, e) }

  lazy val row: P[Seq[Expr]] = "(" ~> rep1sep(expression, ",") <~ ")"

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

  lazy val tableConstraint: P[TableConstraint] =
    opt(kw("CONSTRAINT") ~> identifier) ~ constraintBody ^^ { case name ~ constraint =>
      constraint(name.map(_.name))
    }

  lazy val constraintBody: P[Option[String] => TableConstraint] =
    kw("UNIQUE") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ { cols => (name: Option[String]) => UniqueConstraint(name, cols) }
      | kw("PRIMARY") ~> kw("KEY") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ { cols => (name: Option[String]) => PrimaryKeyConstraint(name, cols) }
      | kw("FOREIGN") ~> kw("KEY") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ~ (kw("REFERENCES") ~> identifier) ~ ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ { 
          case cols ~ table ~ refCols => (name: Option[String]) => ForeignKeyConstraint(name, cols, table, refCols) 
        }

  lazy val createTable: P[Command] =
    kw("CREATE") ~> kw("TABLE") ~> identifier ~ ("(" ~> rep1sep(columnDesc | tableConstraint, ",") <~ ")") ^^ {
      case t ~ items =>
        val columns     = items.collect { case c: ColumnDesc => c }
        val constraints = items.collect { case c: TableConstraint => c }
        CreateTableCommand(t, columns, constraints)
    }

  lazy val dropTable: P[Command] =
    (kw("DROP") ~> kw("TABLE") ~> kw("IF") ~> kw("EXISTS") ~> identifier ^^ { t => 
      DropTableCommand(t, true, false) 
    }) |
    (kw("DROP") ~> kw("TABLE") ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { 
      case t ~ cascade => DropTableCommand(t, false, cascade.contains("CASCADE"))
    })

  lazy val dropIndex: P[Command] =
    (kw("DROP") ~> kw("INDEX") ~> kw("IF") ~> kw("EXISTS") ~> identifier ^^ { name => 
      DropIndexCommand(name, true) 
    }) |
    (kw("DROP") ~> kw("INDEX") ~> identifier ^^ { name => 
      DropIndexCommand(name, false) 
    })

  lazy val dropType: P[Command] =
    (kw("DROP") ~> kw("TYPE") ~> kw("IF") ~> kw("EXISTS") ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { 
      case name ~ cascade => DropTypeCommand(name, true, cascade.contains("CASCADE"))
    }) |
    (kw("DROP") ~> kw("TYPE") ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { 
      case name ~ cascade => DropTypeCommand(name, false, cascade.contains("CASCADE"))
    })

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

  lazy val baseTyp: P[Either[Type, Ident]] =
    kw("BOOLEAN") ^^^ Left(BooleanType)
      | kw("SMALLINT") ^^^ Left(SmallintType)
      | (kw("INT") | kw("INTEGER")) ^^^ Left(IntegerType)
      | kw("BIGINT") ^^^ Left(BigintType)
      | kw("SMALLSERIAL") ^^^ Left(SmallSerialType)
      | kw("SERIAL") ^^^ Left(SerialType)
      | kw("BIGSERIAL") ^^^ Left(BigSerialType)
      | (kw("DOUBLE") ~ opt(kw("PRECISION")) | kw("FLOAT") | kw("REAL")) ^^^ Left(DoubleType)
      | kw("NUMERIC") ~> ("(" ~> integer ~ ("," ~> integer) <~ ")") ^^ { case p ~ s => Left(NumericType(p, s)) }
      | kw("DECIMAL") ~> ("(" ~> integer ~ ("," ~> integer) <~ ")") ^^ { case p ~ s => Left(NumericType(p, s)) }
      | kw("CHAR") ~> ("(" ~> integer <~ ")") ^^ { n => Left(CharType(n)) }
      | kw("JSONB") ^^^ Left(JSONType)
      | kw("JSON") ^^^ Left(JSONType)
      | kw("TIMESTAMP") ~ kw("WITH") ~ kw("TIME") ~ kw("ZONE") ^^^ Left(TimestampTZType)
      | kw("TIMESTAMP") ~ opt(kw("WITHOUT") ~ kw("TIME") ~ kw("ZONE")) ^^^ Left(TimestampType)
      | kw("DATE") ^^^ Left(DateType)
      | kw("TIME") ^^^ Left(TimeType)
      | kw("INTERVAL") ^^^ Left(IntervalType)
      | kw("BYTEA") ^^^ Left(ByteaType)
      | kw("TEXT") ^^^ Left(TextType)
      | kw("UUID") ^^^ Left(UUIDType)
      | identifier ^^ Right.apply

  lazy val typ: P[Either[Type, Ident]] =
    baseTyp ~ opt("[" ~ "]") ^^ {
      case Left(t) ~ Some(_) => Left(ArrayColumnType(t))
      case other ~ _         => other
    }

  lazy val columnDesc: P[ColumnDesc] =
    identifier ~ typ ~ opt(kw("NOT") ~ kw("NULL")) ~ opt(kw("UNIQUE")) ~ opt(kw("DEFAULT") ~> expression) ~ opt(kw("REFERENCES") ~> identifier ~ ("(" ~> identifier <~ ")")) ^^ {
      case c ~ t ~ n ~ u ~ d ~ r =>
        val refs = r.map { case table ~ column => (table, column) }
        ColumnDesc(c, t, n.isDefined, u.isDefined, d, refs)
    }

  lazy val alterTable: P[Command] =
    kw("ALTER") ~> kw("TABLE") ~> identifier ~ tableAlteration ^^ { case t ~ a =>
      AlterTableCommand(t, a)
    }

  lazy val tableAlteration: P[TableAlteration] =
    // RENAME TO (for table) - must come before ADD to avoid conflicts
    kw("RENAME") ~> kw("TO") ~> identifier ^^ { newName =>
      RenameTableAlteration(newName)
    }
    // RENAME COLUMN - must come before ADD to avoid conflicts
    | kw("RENAME") ~> opt(kw("COLUMN")) ~> identifier ~ kw("TO") ~ identifier ^^ { case oldName ~ _ ~ newName =>
        RenameColumnTableAlteration(oldName, newName)
      }
    // ADD COLUMN
    | kw("ADD") ~> opt(kw("COLUMN")) ~> columnDesc ^^ { col =>
        AddColumnTableAlteration(col)
      }
    // ADD CONSTRAINT  
    | kw("ADD") ~> opt(kw("CONSTRAINT") ~> identifier) ~ constraintBody ^^ { case name ~ constraint =>
        AddConstraintTableAlteration(constraint(name.map(_.name)))
      }
    // Legacy - ADD FOREIGN KEY (backward compatibility)
    | kw("ADD") ~> kw("FOREIGN") ~> kw("KEY") ~> ("(" ~> identifier <~ ")") ~ (kw("REFERENCES") ~> identifier) ^^ {
        case fk ~ ref =>
          AddForeignKeyTableAlteration(fk, ref)
      }
    // DROP COLUMN
    | kw("DROP") ~> opt(kw("COLUMN")) ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { case col ~ _ =>
        DropColumnTableAlteration(col)
      }
    // DROP CONSTRAINT
    | kw("DROP") ~> kw("CONSTRAINT") ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { case name ~ _ =>
        DropConstraintTableAlteration(name)
      }
    // ALTER COLUMN
    | kw("ALTER") ~> opt(kw("COLUMN")) ~> identifier ~ columnModification ^^ { case col ~ mod =>
        AlterColumnTableAlteration(col, mod)
      }

  lazy val columnModification: P[ColumnModification] =
    kw("TYPE") ~> typ ^^ { dataType =>
      SetDataTypeColumnModification(dataType)
    }
    | kw("SET") ~> kw("NOT") ~> kw("NULL") ^^ { _ =>
        SetNotNullColumnModification()
      }
    | kw("DROP") ~> kw("NOT") ~> kw("NULL") ^^ { _ =>
        DropNotNullColumnModification()
      }
    | kw("SET") ~> kw("DEFAULT") ~> expression ^^ { expr =>
        SetDefaultColumnModification(expr)
      }
    | kw("DROP") ~> kw("DEFAULT") ^^ { _ =>
        DropDefaultColumnModification()
      }

  lazy val command: P[Command] =
    query ^^ QueryCommand.apply |
      insert |
      createTable |
      dropTable |
      dropIndex |
      dropType |
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

  def parseQuery(input: String): Expr = parse(input, query)

  def parseCommand(input: String): Command = parse(input, command)

  def parseCommands(input: String): Seq[Command] = parse(input, commands)
