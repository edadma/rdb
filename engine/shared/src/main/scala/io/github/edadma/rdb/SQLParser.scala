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
      "<>",
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
      "$",
    )
    reserved ++= Seq(
      "action", "add", "all", "alter", "and", "any", "array", "as", "asc",
      "begin", "between", "bigint", "bigserial", "boolean", "by", "bytea",
      "cascade", "case", "cast", "char", "check", "column", "commit", "constraint",
      "create", "cross", "current_timestamp",
      "database", "date", "deallocate", "decimal", "default", "delete", "desc",
      "distinct", "double", "drop",
      "else", "end", "enum", "except", "exec", "execute", "exists", "extract",
      "false", "first", "float", "foreign", "from", "full",
      "group",
      "having",
      "if", "ilike", "in", "index", "inner", "insert", "int", "integer",
      "intersect", "interval", "into", "is",
      "join", "json", "jsonb",
      "key",
      "last", "lateral", "left", "like", "limit",
      "no", "not", "null", "nulls", "numeric",
      "offset", "on", "or", "order", "outer", "overlaps",
      "precision", "prepare", "primary", "procedure",
      "real", "references", "rename", "restrict", "returning", "right", "rollback",
      "select", "serial", "set", "smallint", "smallserial", "some",
      "table", "text", "then", "time", "timestamp", "to", "transaction",
      "true", "truncate", "type",
      "union", "unique", "update", "uuid",
      "values",
      "when", "where", "with", "without",
      "zone",
    )

    override protected def processIdent(name: String): Token =
      val lower = name.toLowerCase
      if reserved.contains(lower) then Keyword(lower) else Identifier(name)

    case class DecimalLit(chars: String) extends Token {
      override def toString: String = chars
    }

    case class ParameterLit(index: Int) extends Token {
      def chars: String = s"$$$index"
      override def toString: String = chars
    }

    override def token: Parser[Token] = quotedToken | stringToken | parameterToken | decimalToken | super.token

    private def parameterToken: Parser[Token] =
      '$' ~> rep1(digit) ^^ { digits => ParameterLit(digits.mkString.toInt) }

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
      'E' ~> '\'' ~> rep(guard(not('\'')) ~> (('\\' ~ '\'' ^^^ "\\'") | elem("", _ => true))) <~ '\'' ^^ (l =>
        StringLit(unescape(l mkString))
      ) |
      '\'' ~> rep(('\'' ~ '\'' ^^^ '\'') | guard(not('\'')) ~> elem("", _ => true)) <~ '\'' ^^ (l =>
        StringLit(l mkString)
      )

  override val lexical: SQLLexer = new SQLLexer

  import lexical.{DecimalLit, ParameterLit}

  def decimalLit: Parser[String] =
    elem("decimal", _.isInstanceOf[DecimalLit]) ^^ (_.asInstanceOf[DecimalLit].chars)

  def parameterLit: P[Int] =
    elem("parameter", _.isInstanceOf[ParameterLit]) ^^ (_.asInstanceOf[ParameterLit].index)

  type P[+T] = PackratParser[T]

  def kw(s: String): P[String] = keyword(s.toLowerCase) ^^^ s.toUpperCase

  lazy val pos: P[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  lazy val valuesClause: P[Expr] =
    kw("VALUES") ~> rep1sep("(" ~> rep1sep(expression, ",") <~ ")", ",") ^^ ValuesExpr.apply

  lazy val selectCore: P[Expr] =
    kw(
      "SELECT",
    ) ~ opt(kw("DISTINCT")) ~ selectExpressions ~ fromClause ~ whereClause ~ groupByClause ~ havingClause ^^ {
      case _ ~ d ~ p ~ f ~ w ~ g ~ h =>
        SQLSelectExpr(p to ArraySeq, f, w, g, h, None, None, None, distinct = d.isDefined)
    } | valuesClause | "(" ~> compoundSelect <~ ")"

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
        case l ~ Some("LEFT") ~ _ ~ r ~ _ ~ c  => LeftJoinOperator(l, r, c)
        case l ~ Some("RIGHT") ~ _ ~ r ~ _ ~ c => RightJoinOperator(l, r, c)
        case l ~ Some("FULL") ~ _ ~ r ~ _ ~ c  => FullJoinOperator(l, r, c)
        case l ~ _ ~ _ ~ r ~ _ ~ c             => InnerJoinOperator(l, r, c)
      } | source

  lazy val source: P[Expr] =
    kw("LATERAL") ~> ("(" ~> query <~ ")") ~ opt(opt(kw("AS")) ~> identifier ~ opt("(" ~> rep1sep(identifier, ",") <~ ")")) ^^ {
      case q ~ None                  => LateralExpr(q)
      case q ~ Some(a ~ None)        => AliasOperator(LateralExpr(q), a)
      case q ~ Some(a ~ Some(cols))  => ColumnAliasOperator(LateralExpr(q), a, cols)
    } |
    (table | valuesClause | ("(" ~> query <~ ")")) ~ opt(opt(kw("AS")) ~> identifier ~ opt("(" ~> rep1sep(identifier, ",") <~ ")")) ^^ {
      case s ~ None                  => s
      case s ~ Some(a ~ None)        => AliasOperator(s, a)
      case s ~ Some(a ~ Some(cols))  => ColumnAliasOperator(s, a, cols)
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
      expression ~ "=" ~ (kw("ANY") | kw("SOME")) ~ "(" ~ kw("ARRAY") ~ "[" ~ expressions ~ "]" ~ ")" ^^ {
        case e ~ _ ~ _ ~ _ ~ _ ~ _ ~ es ~ _ ~ _ => InSeqExpr(e, "IN", es)
      } |
      expression ~ "=" ~ (kw("ANY") | kw("SOME")) ~ "(" ~ query ~ ")" ^^ {
        case e ~ _ ~ _ ~ _ ~ q ~ _ => InQueryExpr(e, "IN", q)
      } |
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
      ("(" ~> expression ~ ("," ~> expression) <~ ")") ~ kw("OVERLAPS") ~ ("(" ~> expression ~ ("," ~> expression) <~ ")") ^^ {
        case (s1 ~ e1) ~ _ ~ (s2 ~ e2) => OverlapsExpr(s1, e1, s2, e2)
      } |
      "(" ~> booleanExpression <~ ")",
  )

  lazy val isNull: P[String] =
    kw("IS") ~ kw("NULL") ^^^ "IS NULL" | kw("IS") ~ kw("NOT") ~ kw("NULL") ^^^ "IS NOT NULL"

  lazy val in: P[String] = kw("NOT") ~ kw("IN") ^^^ "NOT IN" | kw("IN")

  lazy val comparison: P[String] =
    "<=" | ">=" | "<>" ^^^ "!=" | "<" | ">" | "=" | "!=" | kw("LIKE") | kw("ILIKE") | (kw("NOT") ~ kw("LIKE") ^^^ "NOT LIKE" | kw(
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
    pos ~ kw("CURRENT_TIMESTAMP") ^^ { case p ~ v => VariableExpr(Ident(v).setPos(p)) },
  )

  lazy val integer: P[Int] = numericLit ^^ (_.toInt)

  lazy val decimal: P[Double] = decimalLit ^^ (_.toDouble)

  lazy val primary: P[Expr] = positioned(
    decimal ^^ (n => NumberExpr(n)) |
      integer ^^ (n => NumberExpr(n)) |
      parameterLit ^^ (n => ParameterExpr(n)) |
      stringLit ^^ StringExpr.apply |
      kw("NULL") ^^^ NullExpr() |
      kw("ARRAY") ~> "[" ~> repsep(expression, ",") <~ "]" ^^ ArrayExpr.apply |
      kw("CAST") ~> "(" ~> expression ~ kw("AS") ~ castType <~ ")" ^^ { case e ~ _ ~ t => CastExpr(e, t) } |
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
    kw("INSERT") ~> kw("INTO") ~> identifier ~ opt("(" ~> rep1sep(identifier, ",") <~ ")") ~ kw("VALUES") ~ rep1sep(
      row,
      ",",
    ) ~ opt(
      kw("RETURNING") ~> identifier,
    ) ^^ { case t ~ cs ~ _ ~ rs ~ ret =>
      InsertCommand(t, cs, rs, ret)
    } |
    kw("INSERT") ~> kw("INTO") ~> identifier ~ opt("(" ~> rep1sep(identifier, ",") <~ ")") ~ query ~ opt(
      kw("RETURNING") ~> identifier,
    ) ^^ { case t ~ cs ~ q ~ ret =>
      InsertSelectCommand(t, cs, q, ret)
    }

  lazy val tableConstraint: P[TableConstraint] =
    opt(kw("CONSTRAINT") ~> identifier) ~ constraintBody ^^ { case name ~ constraint =>
      constraint(name.map(_.name))
    }

  lazy val referentialAction: P[ReferentialAction] =
    kw("CASCADE") ^^^ ReferentialAction.Cascade
      | kw("RESTRICT") ^^^ ReferentialAction.Restrict
      | kw("SET") ~ kw("NULL") ^^^ ReferentialAction.SetNull
      | kw("NO") ~ kw("ACTION") ^^^ ReferentialAction.NoAction

  lazy val onDeleteClause: P[ReferentialAction] = kw("ON") ~> kw("DELETE") ~> referentialAction
  lazy val onUpdateClause: P[ReferentialAction] = kw("ON") ~> kw("UPDATE") ~> referentialAction

  lazy val constraintBody: P[Option[String] => TableConstraint] =
    kw("UNIQUE") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ { cols => (name: Option[String]) => UniqueConstraint(name, cols) }
      | kw("PRIMARY") ~> kw("KEY") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ { cols => (name: Option[String]) => PrimaryKeyConstraint(name, cols) }
      | kw("FOREIGN") ~> kw("KEY") ~> ("(" ~> rep1sep(identifier, ",") <~ ")") ~ (kw("REFERENCES") ~> identifier) ~ ("(" ~> rep1sep(identifier, ",") <~ ")") ~ opt(onDeleteClause) ~ opt(onUpdateClause) ^^ {
          case cols ~ table ~ refCols ~ onDel ~ onUpd => (name: Option[String]) =>
            ForeignKeyConstraint(name, cols, table, refCols, onDel.getOrElse(ReferentialAction.NoAction), onUpd.getOrElse(ReferentialAction.NoAction))
        }

  lazy val createTable: P[Command] =
    kw("CREATE") ~> kw("TABLE") ~> opt(kw("IF") ~> kw("NOT") ~> kw("EXISTS")) ~ identifier ~ ("(" ~> rep1sep(columnDesc | tableConstraint, ",") <~ ")") ^^ {
      case ine ~ t ~ items =>
        val columns     = items.collect { case c: ColumnDesc => c }
        val constraints = items.collect { case c: TableConstraint => c }
        CreateTableCommand(t, columns, constraints, ine.isDefined)
    }

  lazy val dropTable: P[Command] =
    (kw("DROP") ~> kw("TABLE") ~> kw("IF") ~> kw("EXISTS") ~> identifier ^^ { t => 
      DropTableCommand(t, true, false) 
    }) |
    (kw("DROP") ~> kw("TABLE") ~> identifier ~ opt(kw("CASCADE") | kw("RESTRICT")) ^^ { 
      case t ~ cascade => DropTableCommand(t, false, cascade.contains("CASCADE"))
    })

  lazy val createIndex: P[Command] =
    kw("CREATE") ~> opt(kw("UNIQUE")) ~ kw("INDEX") ~ identifier ~ kw("ON") ~ identifier ~ ("(" ~> rep1sep(identifier, ",") <~ ")") ^^ {
      case u ~ _ ~ name ~ _ ~ table ~ cols =>
        CreateIndexCommand(name, table, cols, u.isDefined)
    }

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
    kw("UPDATE") ~> identifier ~ kw("SET") ~ rep1sep(set, ",") ~
      opt(kw("FROM") ~> rep1sep(sources, ",")) ~
      opt(kw("WHERE") ~> booleanExpression) ^^ {
      case t ~ _ ~ ss ~ f ~ c =>
        UpdateCommand(t, ss, f, c)
    }

  lazy val delete: P[Command] =
    kw("DELETE") ~> kw("FROM") ~> identifier ~ opt(kw("WHERE") ~> booleanExpression) ^^ { case t ~ c =>
      DeleteCommand(t, c)
    }

  lazy val truncate: P[Command] =
    kw("TRUNCATE") ~> opt(kw("TABLE")) ~> identifier ^^ TruncateCommand.apply

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
    identifier ~ typ ~ opt(kw("NOT") ~ kw("NULL")) ~ opt(kw("UNIQUE")) ~ opt(kw("DEFAULT") ~> expression) ~ opt(kw("REFERENCES") ~> identifier ~ ("(" ~> identifier <~ ")") ~ opt(onDeleteClause) ~ opt(onUpdateClause)) ^^ {
      case c ~ t ~ n ~ u ~ d ~ r =>
        val refs = r.map { case table ~ column ~ onDel ~ onUpd => (table, column, onDel.getOrElse(ReferentialAction.NoAction), onUpd.getOrElse(ReferentialAction.NoAction)) }
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

  lazy val prepare: P[Command] =
    kw("PREPARE") ~> identifier ~ kw("AS") ~ command ^^ { case name ~ _ ~ cmd =>
      PrepareCommand(name, Seq(cmd))
    }

  lazy val executeCmd: P[Command] =
    kw("EXECUTE") ~> identifier ~ opt("(" ~> rep1sep(expression, ",") <~ ")") ^^ { case name ~ params =>
      ExecuteCommand(name, params.getOrElse(Nil))
    }

  lazy val deallocate: P[Command] =
    kw("DEALLOCATE") ~> opt(kw("PREPARE")) ~> identifier ^^ DeallocateCommand.apply

  lazy val beginCmd: P[Command] = kw("BEGIN") ~> opt(kw("TRANSACTION")) ^^^ BeginCommand
  lazy val commitCmd: P[Command] = kw("COMMIT") ~> opt(kw("TRANSACTION")) ^^^ CommitCommand
  lazy val rollbackCmd: P[Command] = kw("ROLLBACK") ~> opt(kw("TRANSACTION")) ^^^ RollbackCommand

  lazy val command: P[Command] =
    beginCmd |
      commitCmd |
      rollbackCmd |
      prepare |
      executeCmd |
      deallocate |
      query ^^ QueryCommand.apply |
      insert |
      createTable |
      createIndex |
      dropTable |
      dropIndex |
      dropType |
      createType |
      update |
      delete |
      truncate |
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
