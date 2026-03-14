package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input.{Position, Positional}
import fastparse._

object SQLParser:

  // ── Position bridging ──────────────────────────────────────────────
  // Input is carried through the parser context (ParserInput) rather
  // than a mutable var, so concurrent parses are safe.

  private class IndexPosition(input: ParserInput, idx: Int) extends Position:
    private lazy val computed: (Int, Int, String) =
      var line = 1
      var col = 1
      var lineStart = 0
      var i = 0
      while i < idx && i < input.length do
        if input(i) == '\n' then
          line += 1
          col = 1
          lineStart = i + 1
        else
          col += 1
        i += 1
      var lineEnd = lineStart
      while lineEnd < input.length && input(lineEnd) != '\n' do lineEnd += 1
      val contents = input.slice(lineStart, lineEnd)
      (line, col, contents)

    def line: Int = computed._1
    def column: Int = computed._2
    protected def lineContents: String = computed._3

  // Loc pairs the parser's input with an index — replaces the old mutable currentInput.
  // Wrapping in a class prevents fastparse's tuple flattening from merging it with ~.
  private class Loc(val input: ParserInput, val idx: Int)

  // Drop-in replacement for Index that also captures the input from the parser context.
  private def Idx[p: P]: P[Loc] =
    val input = summon[P[p]].input
    P(Index).map(i => Loc(input, i))

  private def mkPos(loc: Loc): Position = new IndexPosition(loc.input, loc.idx)

  private def pos[T <: Positional](loc: Loc, t: T): T =
    t.setPos(mkPos(loc))
    t

  // ── Whitespace handler ─────────────────────────────────────────────

  implicit val whitespace: fastparse.Whitespace = { implicit ctx: ParsingRun[?] =>
    val input = ctx.input
    var idx = ctx.index
    val length = input.length
    var continue = true

    while continue && idx < length do
      val c = input(idx)
      if c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' then
        idx += 1
      else if c == '-' && idx + 1 < length && input(idx + 1) == '-' then
        idx += 2
        while idx < length && input(idx) != '\n' do idx += 1
      else if c == '/' && idx + 1 < length && input(idx + 1) == '*' then
        idx += 2
        var depth = 1
        while idx < length && depth > 0 do
          if input(idx) == '/' && idx + 1 < length && input(idx + 1) == '*' then
            depth += 1
            idx += 2
          else if input(idx) == '*' && idx + 1 < length && input(idx + 1) == '/' then
            depth -= 1
            idx += 2
          else
            idx += 1
        if depth > 0 then
          ctx.freshFailure(idx)
          ctx.asInstanceOf[ParsingRun[Unit]]
        else
          continue = false // re-loop to handle whitespace after comment
          continue = true
      else
        continue = false

    ctx.freshSuccessUnit(idx)
    ctx.asInstanceOf[ParsingRun[Unit]]
  }

  // ── Character predicates ───────────────────────────────────────────

  private def identStartChar(c: Char): Boolean = c.isLetter || c == '_'
  private def identChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  // ── Reserved words ─────────────────────────────────────────────────

  private val reservedWords: Set[String] = Set(
    "action", "add", "all", "alter", "and", "any", "array", "as", "asc",
    "begin", "between", "bigint", "bigserial", "boolean", "by", "bytea",
    "cascade", "case", "cast", "char", "check", "column", "commit", "conflict", "constraint",
    "copy", "create", "cross", "current_date", "current_time", "current_timestamp",
    "database", "date", "deallocate", "decimal", "default", "delete", "desc",
    "distinct", "do", "double", "drop",
    "else", "end", "enum", "except", "exec", "execute", "exists", "explain", "extract",
    "false", "first", "float", "for", "foreign", "from", "full",
    "group",
    "having",
    "if", "ilike", "in", "index", "inner", "insert", "int", "integer",
    "indexes", "intersect", "interval", "into", "is",
    "join", "json", "jsonb",
    "last", "lateral", "like", "limit",
    "no", "not", "nothing", "null", "nulls", "numeric",
    "offset", "on", "or", "order", "outer", "overlay", "overlaps",
    "placing", "precision", "prepare", "primary", "procedure",
    "real", "references", "rename", "restrict", "returning", "rollback",
    "select", "serial", "set", "show", "smallint", "smallserial", "some", "symmetric",
    "columns",
    "table", "text", "then", "time", "timetz", "timestamp", "to", "transaction",
    "true", "truncate", "type",
    "union", "unique", "unknown", "update", "uuid",
    "values", "varchar",
    "when", "where", "with", "without",
    "zone",
  )

  // ── Keywords and identifiers ───────────────────────────────────────

  // kw returns P[Unit] — matches keyword case-insensitively, ensures not followed by ident char
  private def kw[p: P](s: String): P[Unit] = {
    import NoWhitespace._
    P(IgnoreCase(s) ~ !CharPred(identChar))
  }

  // identRaw returns P[String] — unquoted identifier, lowercased, not a reserved word
  private def identRaw[p: P]: P[String] = {
    import NoWhitespace._
    P((CharPred(identStartChar) ~ CharsWhile(identChar, 0)).!)
      .map(_.toLowerCase)
      .filter(!reservedWords.contains(_))
  }

  // quotedIdent returns P[String] — double-quoted identifier, preserves case
  private def quotedIdent[p: P]: P[String] = {
    import NoWhitespace._
    P("\"" ~ CharsWhile(_ != '"', 1).! ~ "\"")
  }

  // ident returns P[String]
  private def ident[p: P]: P[String] = P(quotedIdent | identRaw)

  // anyIdent: like identRaw but allows reserved words (for schema-qualified names)
  private def anyIdent[p: P]: P[String] = {
    import NoWhitespace._
    P((CharPred(identStartChar) ~ CharsWhile(identChar, 0)).!)
      .map(_.toLowerCase)
  }

  // Words that cannot be used as bare aliases (without AS) because they are clause/join keywords.
  // These are non-reserved in general but ambiguous in alias position.
  private val aliasExcluded: Set[String] = reservedWords ++ Set(
    "left", "right", "inner", "full", "cross", "join", "outer",
    "natural", "using", "lateral",
  )

  // aliasIdent: identifier excluding clause keywords — for bare (without AS) alias positions
  private def aliasIdent[p: P]: P[String] = {
    import NoWhitespace._
    P((CharPred(identStartChar) ~ CharsWhile(identChar, 0)).!)
      .map(_.toLowerCase)
      .filter(!aliasExcluded.contains(_))
  }

  // anyIdentOrQuoted: like anyIdent but also allows double-quoted identifiers
  private def anyIdentOrQuoted[p: P]: P[String] = P(quotedIdent | anyIdent)

  // identifier returns P[Ident] with position
  private def identifier[p: P]: P[Ident] =
    P(Idx ~ ident).map((loc, name) => pos(loc, Ident(name)))

  // tableIdent: schema-qualified table name — preserves schema prefix as "schema.table"
  private def tableIdent[p: P]: P[Ident] =
    P(Idx ~ ident ~ ("." ~ anyIdentOrQuoted).?).map {
      case (loc, schema, Some(name)) => pos(loc, Ident(s"$schema.$name"))
      case (loc, name, None)         => pos(loc, Ident(name))
    }

  // ── Literals ───────────────────────────────────────────────────────

  private def digits[p: P]: P[String] = {
    import NoWhitespace._
    P(CharsWhileIn("0-9", 1).!)
  }

  private def exponent[p: P]: P[String] = {
    import NoWhitespace._
    P((CharIn("eE") ~ CharIn("+\\-").? ~ CharsWhileIn("0-9", 1)).!)
  }

  // decimalLit returns P[String] — the raw decimal text
  private def decimalLit[p: P]: P[String] = {
    import NoWhitespace._
    P(
      (CharsWhileIn("0-9", 1) ~ "." ~ CharsWhileIn("0-9", 1) ~ exponent.?).!
      | ("." ~ CharsWhileIn("0-9", 1) ~ exponent.?).!
      | (CharsWhileIn("0-9", 1) ~ exponent).!
    )
  }

  // integerLit returns P[Int]
  private def integerLit[p: P]: P[Int] = {
    import NoWhitespace._
    P(CharsWhileIn("0-9", 1).!).map(_.toInt)
  }

  // parameterLit returns P[Int] — the parameter index
  private def parameterLit[p: P]: P[Int] = {
    import NoWhitespace._
    P("$" ~ CharsWhileIn("0-9", 1).!).map(_.toInt)
  }

  // sqlStringLit returns P[String] — standard SQL string with '' escape
  private def sqlStringLit[p: P]: P[String] = {
    import NoWhitespace._
    P("'" ~ (("''" | (!"'" ~ AnyChar)).rep.!) ~ "'").map(_.replace("''", "'"))
  }

  // eStringLit returns P[String] — E'...' string with backslash escapes
  private def eStringLit[p: P]: P[String] = {
    import NoWhitespace._
    P(IgnoreCase("e") ~ "'" ~ (("\\" ~ AnyChar) | (!"'" ~ AnyChar)).rep.! ~ "'").map(unescape)
  }

  // stringLit returns P[String]
  private def stringLit[p: P]: P[String] = P(eStringLit | sqlStringLit)

  // integer returns P[Int] (with whitespace)
  private def integer[p: P]: P[Int] = P(integerLit)

  // ── Types ──────────────────────────────────────────────────────────

  private def baseTypNumeric[p: P]: P[Either[Type, Ident]] =
    P(
      kw("boolean").map(_ => Left(BooleanType))
      | kw("smallint").map(_ => Left(SmallintType))
      | (kw("integer") | kw("int")).map(_ => Left(IntegerType))
      | kw("bigint").map(_ => Left(BigintType))
      | kw("smallserial").map(_ => Left(SmallSerialType))
      | kw("serial").map(_ => Left(SerialType))
      | kw("bigserial").map(_ => Left(BigSerialType))
      | (kw("double") ~ kw("precision").? | kw("float") | kw("real")).map(_ => Left(DoubleType))
      | (kw("numeric") ~ "(" ~ integer ~ ("," ~ integer).? ~ ")").map { case (p, s) => Left(NumericType(p, s.getOrElse(0))) }
      | kw("numeric").map(_ => Left(NumericType(0, 0)))
      | (kw("decimal") ~ "(" ~ integer ~ ("," ~ integer).? ~ ")").map { case (p, s) => Left(NumericType(p, s.getOrElse(0))) }
      | kw("decimal").map(_ => Left(NumericType(0, 0)))
    )

  private def baseTypString[p: P]: P[Either[Type, Ident]] =
    P(
      (kw("char") ~ "(" ~ integer ~ ")").map(n => Left(CharType(n)))
      | (kw("varchar") ~ "(" ~ integer ~ ")").map(n => Left(VarcharType(n)))
      | kw("varchar").map(_ => Left(TextType))
      | kw("jsonb").map(_ => Left(JSONType))
      | kw("json").map(_ => Left(JSONType))
      | kw("bytea").map(_ => Left(ByteaType))
      | kw("text").map(_ => Left(TextType))
      | kw("uuid").map(_ => Left(UUIDType))
    )

  private def baseTypTemporal[p: P]: P[Either[Type, Ident]] =
    P(
      (kw("timestamp") ~ kw("with") ~ kw("time") ~ kw("zone")).map(_ => Left(TimestampTZType))
      | (kw("timestamp") ~ (kw("without") ~ kw("time") ~ kw("zone")).?).map(_ => Left(TimestampType))
      | kw("date").map(_ => Left(DateType))
      | kw("timetz").map(_ => Left(TimeTZType))
      | (kw("time") ~ kw("with") ~ kw("time") ~ kw("zone")).map(_ => Left(TimeTZType))
      | kw("time").map(_ => Left(TimeType))
      | kw("interval").map(_ => Left(IntervalType))
    )

  private def baseTyp[p: P]: P[Either[Type, Ident]] =
    P(baseTypNumeric | baseTypString | baseTypTemporal | identifier.map(Right(_)))

  private def typ[p: P]: P[Either[Type, Ident]] =
    P(baseTyp ~ ("[" ~ "]").!.?).map { case (base, arr) =>
      if arr.isDefined then
        base match
          case Left(t) => Left(ArrayColumnType(t))
          case other => other
      else base
    }

  private def castType[p: P]: P[Type] =
    typ.flatMap {
      case Left(t) => Pass.map(_ => t)
      case Right(_) => Fail.opaque("cannot cast to custom type")
    }

  // ── Expression chain ───────────────────────────────────────────────

  // All binary chains: left ~ (op ~ right).rep → foldLeft
  private def expression[p: P]: P[Expr] = P(orExpression)

  private def concatenation[p: P]: P[Expr] =
    P(bitwise ~ ("||".! ~ bitwise).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def bitwiseOp[p: P]: P[String] = {
    import NoWhitespace._
    P("<<".! | ">>".! | ("&" ~ !"&").!.map(_ => "&") | ("|" ~ !"|").!.map(_ => "|") | ("#" ~ !">").!.map(_ => "#"))
  }

  private def bitwise[p: P]: P[Expr] =
    P(additive ~ (bitwiseOp ~ additive).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def additiveOp[p: P]: P[String] = {
    import NoWhitespace._
    P("+".! | ("-" ~ !("-" | ">")).!.map(_ => "-"))
  }

  private def additive[p: P]: P[Expr] =
    P(multiplicative ~ (additiveOp ~ multiplicative).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def multiplicativeOp[p: P]: P[String] = P("*".! | "/".! | "%".!)

  private def multiplicative[p: P]: P[Expr] =
    P(exponentiation ~ (multiplicativeOp ~ exponentiation).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def exponentiation[p: P]: P[Expr] =
    P(castExpression ~ ("^".! ~ castExpression).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def jsonAccessOp[p: P]: P[String] = {
    import NoWhitespace._
    P("->>".! | ("->" ~ !">").!.map(_ => "->") | "#>>".! | ("#>" ~ !">").!.map(_ => "#>"))
  }

  private sealed trait PostfixOp
  private case class CastOp(t: Type) extends PostfixOp
  private case class JsonOp(op: String, rhs: Expr) extends PostfixOp

  private def castExpression[p: P]: P[Expr] =
    P(primary ~ (("::" ~ castType).map(CastOp(_)) | (jsonAccessOp ~ primary).map((op, e) => JsonOp(op, e))).rep).map { case (first, ops) =>
      ops.foldLeft(first) {
        case (l, CastOp(t))      => CastExpr(l, t).setPos(l.pos).asInstanceOf[Expr]
        case (l, JsonOp(op, r))  => BinaryExpr(l, op, r).setPos(l.pos).asInstanceOf[Expr]
      }
    }

  // ── Extract field ──────────────────────────────────────────────────

  private def extractField[p: P]: P[String] =
    P(
      kw("timestamp").map(_ => "timestamp")
      | kw("time").map(_ => "time")
      | kw("date").map(_ => "date")
      | ident.map(_.toLowerCase)
    )

  // ── Primary expressions ────────────────────────────────────────────

  private def primaryLiterals[p: P]: P[Expr] =
    P(
      decimalPrimary
      | integerPrimary
      | parameterPrimary
      | stringPrimary
      | nullPrimary
      | booleanLiteral
      | jsonLiteral
    )

  // INTERVAL '1 day' — typed interval literal, desugared to CAST('...' AS INTERVAL)
  private def intervalLiteral[p: P]: P[Expr] =
    P(Idx ~ kw("interval") ~ stringLit).map((loc, s) =>
      pos(loc, CastExpr(StringExpr(s), IntervalType))
    )

  private def primaryKeyword[p: P]: P[Expr] =
    P(
      arrayPrimary
      | castPrimary
      | extractPrimary
      | overlayPrimary
      | intervalLiteral
      | tableConstructorPrimary
    )

  private def primaryComplex[p: P]: P[Expr] =
    P(
      application
      | column
      | variable
      | caseExpression.map(_.asInstanceOf[Expr])
      | unaryMinusPrimary
      | bitwiseNotPrimary
      | subqueryPrimary
      | parenExpr
    )

  private def primary[p: P]: P[Expr] = P(primaryLiterals | primaryKeyword | primaryComplex)

  private def decimalPrimary[p: P]: P[Expr] =
    P(Idx ~ decimalLit).map((loc, s) => pos(loc, NumberExpr(s.toDouble)))

  // Parse integer value as Long to handle BIGINT-range literals
  private def longLit[p: P]: P[Long] = {
    import NoWhitespace._
    P(CharsWhileIn("0-9", 1).!).map(_.toLong)
  }

  private def integerPrimary[p: P]: P[Expr] =
    P(Idx ~ longLit).map((loc, n) =>
      if n >= Int.MinValue && n <= Int.MaxValue then pos(loc, NumberExpr(n.toInt))
      else pos(loc, NumberExpr(n))
    )

  private def parameterPrimary[p: P]: P[Expr] =
    P(Idx ~ parameterLit).map((loc, n) => pos(loc, ParameterExpr(n)))

  private def stringPrimary[p: P]: P[Expr] =
    P(Idx ~ stringLit).map((loc, s) => pos(loc, StringExpr(s)))

  // Index ~ kw("null") => just Int (kw returns Unit, dropped)
  private def nullPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("null")).map(loc => pos(loc, NullExpr()))

  // Index ~ kw("array") ~ "[" ~ ... ~ "]" => (Int, Seq[Expr])
  private def arrayPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("array") ~ "[" ~ expression.rep(sep = ",") ~ "]").map((loc, elems) =>
      pos(loc, ArrayExpr(elems))
    )

  // CAST(expr AS type) => (Int, Expr, Type)
  private def castPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("cast") ~ "(" ~ expression ~ kw("as") ~ castType ~ ")").map((loc, e, t) =>
      pos(loc, CastExpr(e, t))
    )

  // EXTRACT(field FROM expr) => (Int, String, Expr)
  private def extractPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("extract") ~ "(" ~ extractField ~ kw("from") ~ expression ~ ")").map((loc, field, source) =>
      pos(loc, ApplyExpr(Ident("date_part"), Seq(StringExpr(field), source)))
    )

  // OVERLAY(s PLACING repl FROM start [FOR count]) => complex
  private def overlayPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("overlay") ~ "(" ~ expression ~ kw("placing") ~ expression ~ kw("from") ~ expression ~ (kw("for") ~ expression).? ~ ")").map {
      case (loc, s, repl, start, Some(count)) => pos(loc, ApplyExpr(Ident("overlay"), Seq(s, repl, start, count)))
      case (loc, s, repl, start, None) => pos(loc, ApplyExpr(Ident("overlay"), Seq(s, repl, start)))
    }

  private def frameBound[p: P]: P[FrameBound] =
    P(kw("unbounded") ~ kw("preceding")).map(_ => UnboundedPreceding) |
    P(kw("unbounded") ~ kw("following")).map(_ => UnboundedFollowing) |
    P(kw("current") ~ kw("row")).map(_ => CurrentRow) |
    P(intLit ~ kw("preceding")).map(n => Preceding(n)) |
    P(intLit ~ kw("following")).map(n => Following(n))

  private def intLit[p: P]: P[Int] = {
    import NoWhitespace._
    P(CharIn("0-9").rep(1).!).map(_.toInt)
  }

  private def frameClause[p: P]: P[FrameSpec] =
    P(kw("rows") ~ kw("between") ~ frameBound ~ kw("and") ~ frameBound).map {
      case (start, end) => FrameSpec(start, end)
    }

  private def windowSpecClause[p: P]: P[(Seq[Expr], Seq[OrderBy], Option[FrameSpec])] =
    P((kw("partition") ~ kw("by") ~ expression.rep(1, sep = ",")).? ~
      (kw("order") ~ kw("by") ~ orderByItem.rep(1, sep = ",")).? ~
      frameClause.?).map {
      case (partBy, ordBy, frame) => (partBy.map(_.toSeq).getOrElse(Nil), ordBy.map(_.toSeq).getOrElse(Nil), frame)
    }

  // func(args...) [FILTER (WHERE ...)] [OVER (...)]
  private def application[p: P]: P[Expr] =
    P(Idx ~ identifier ~ "(" ~ (expression | star).rep(sep = ",") ~ ")" ~
      (kw("filter") ~ "(" ~ kw("where") ~ expression ~ ")").? ~
      (kw("over") ~ "(" ~ windowSpecClause ~ ")").?).map {
      case (loc, f, as, filter, Some((partBy, ordBy, frame))) =>
        pos(loc, WindowExpr(ApplyExpr(f, as, filter), partBy, ordBy, frame))
      case (loc, f, as, filter, None) =>
        pos(loc, ApplyExpr(f, as, filter))
    }

  // table.column or just column — identifier ~ ("." ~ identifier).? => (Int, Ident, Option[Ident])
  private def column[p: P]: P[ColumnExpr] =
    P(Idx ~ identifier ~ ("." ~ identifier).?).map {
      case (loc, c, None) => pos(loc, ColumnExpr(None, c))
      case (loc, t, Some(c)) => pos(loc, ColumnExpr(Some(t), c))
    }

  // CURRENT_TIMESTAMP, CURRENT_DATE, CURRENT_TIME — SQL standard variables (with optional parens)
  private def variable[p: P]: P[VariableExpr] =
    P(
      (Idx ~ kw("current_timestamp") ~ ("(" ~ ")").?).map(loc => pos(loc, VariableExpr(pos(loc, Ident("CURRENT_TIMESTAMP")))))
      | (Idx ~ kw("current_date") ~ ("(" ~ ")").?).map(loc => pos(loc, VariableExpr(pos(loc, Ident("CURRENT_DATE")))))
      | (Idx ~ kw("current_time") ~ ("(" ~ ")").?).map(loc => pos(loc, VariableExpr(pos(loc, Ident("CURRENT_TIME")))))
    )

  private def unaryMinusPrimary[p: P]: P[Expr] =
    P(Idx ~ "-" ~ primary).map((loc, e) => pos(loc, UnaryExpr("-", e)))

  private def bitwiseNotPrimary[p: P]: P[Expr] =
    P(Idx ~ "~" ~ primary).map((loc, e) => pos(loc, UnaryExpr("~", e)))

  // TABLE(query)
  private def tableConstructorPrimary[p: P]: P[Expr] =
    P(Idx ~ kw("table") ~ "(" ~ query ~ ")").map((loc, q) => pos(loc, TableConstructorExpr(q)))

  // (query) as subquery — only when followed by set ops, order, limit, offset, ), ;, or end
  private def subqueryPrimary[p: P]: P[Expr] =
    P("(" ~ query ~ ")" ~ &(kw("union") | kw("intersect") | kw("except") | kw("order") | kw("limit") | kw("offset") | kw("as") | kw("then") | kw("else") | kw("end") | kw("when") | kw("and") | kw("or") | kw("from") | kw("where") | kw("group") | kw("having") | kw("on") | kw("is") | kw("not") | kw("in") | kw("between") | kw("like") | kw("ilike") | "," | ")" | ";" | End)).map(q =>
      SubqueryExpr(q).setPos(q.pos).asInstanceOf[Expr]
    )

  private def parenExpr[p: P]: P[Expr] = P("(" ~ expression ~ ")")

  // ── Literal (for JSON values) ──────────────────────────────────────

  private def literal[p: P]: P[Expr] =
    P(
      booleanLiteral
      | jsonLiteral
      | decimalPrimary
      | integerPrimary
      | stringPrimary
      | nullPrimary
      | (Idx ~ "-" ~ primary).map((loc, e) => pos(loc, UnaryExpr("-", e)))
    )

  private def orExpression[p: P]: P[Expr] =
    P(andExpression ~ (kw("or") ~ andExpression).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, r) => BinaryExpr(l, "OR", r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def andExpression[p: P]: P[Expr] =
    P(notExpression ~ (kw("and") ~ notExpression).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, r) => BinaryExpr(l, "AND", r).setPos(l.pos).asInstanceOf[Expr] }
    }

  private def notExpression[p: P]: P[Expr] =
    P(
      (Idx ~ kw("not") ~ notExpression).map((loc, e) => pos(loc, UnaryExpr("NOT", e)))
      | comparisonExpression
    )

  // ── Comparison / suffix layer ────────────────────────────────────

  private def comparisonExpression[p: P]: P[Expr] =
    P(existsExpr | overlapsExpr | (concatenation ~ booleanSuffix.?).map {
      case (e, Some(f)) => f(e)
      case (e, None) => e
    })

  private def existsExpr[p: P]: P[Expr] =
    P(Idx ~ kw("exists") ~ "(" ~ query ~ ")").map((loc, q) => pos(loc, ExistsExpr(q)))

  private def overlapsExpr[p: P]: P[Expr] =
    P(Idx ~ "(" ~ expression ~ "," ~ expression ~ ")" ~ kw("overlaps") ~ "(" ~ expression ~ "," ~ expression ~ ")").map {
      (loc, s1, e1, s2, e2) => pos(loc, OverlapsExpr(s1, e1, s2, e2))
    }


  private def booleanSuffix[p: P]: P[Expr => Expr] =
    P(jsonbOpSuffix | quantifiedSuffix | isDistinctSuffix | comparisonSuffix | betweenSuffix | isNullSuffix | inSuffix)

  // @>, <@, &&, ?&, ?|, ? — operator ~ concatenation => (String, Expr)
  private def jsonbOpSuffix[p: P]: P[Expr => Expr] =
    P(("@>".! | "<@".! | "&&".! | "?&".! | "?|".! | "?".!) ~ concatenation).map { case (op, right) =>
      (left: Expr) => BinaryExpr(left, op, right).setPos(left.pos).asInstanceOf[Expr]
    }

  private def quantifiedSuffix[p: P]: P[Expr => Expr] =
    P(eqAnyArraySuffix | eqAnyQuerySuffix | cmpQuantifiedSuffix)

  // = ANY/SOME(ARRAY[...]) => Seq[Expr]
  private def eqAnyArraySuffix[p: P]: P[Expr => Expr] =
    P("=" ~ (kw("any") | kw("some")) ~ "(" ~ kw("array") ~ "[" ~ expressions ~ "]" ~ ")").map { es =>
      (left: Expr) => InSeqExpr(left, "IN", es).setPos(left.pos).asInstanceOf[Expr]
    }

  // = ANY/SOME(query) => Expr
  private def eqAnyQuerySuffix[p: P]: P[Expr => Expr] =
    P("=" ~ (kw("any") | kw("some")) ~ "(" ~ query ~ ")").map { q =>
      (left: Expr) => InQueryExpr(left, "IN", q).setPos(left.pos).asInstanceOf[Expr]
    }

  // comparison ANY/SOME/ALL(expr) => (String, String, Expr)
  private def cmpQuantifiedSuffix[p: P]: P[Expr => Expr] =
    P(comparison ~ (kw("any") | kw("some") | kw("all")).!.map(_.toUpperCase) ~ "(" ~ concatenation ~ ")").map { case (cmp, quant, arr) =>
      val q = if quant == "SOME" then "ANY" else quant
      (left: Expr) => QuantifiedCompareExpr(left, cmp, q, arr).setPos(left.pos).asInstanceOf[Expr]
    }

  private def isDistinctSuffix[p: P]: P[Expr => Expr] =
    P(
      (kw("is") ~ kw("not") ~ kw("distinct") ~ kw("from") ~ concatenation).map { right =>
        (left: Expr) => BinaryExpr(left, "IS NOT DISTINCT FROM", right).setPos(left.pos).asInstanceOf[Expr]
      }
      | (kw("is") ~ kw("distinct") ~ kw("from") ~ concatenation).map { right =>
          (left: Expr) => BinaryExpr(left, "IS DISTINCT FROM", right).setPos(left.pos).asInstanceOf[Expr]
        }
    )

  // comparison ~ concatenation => (String, Expr)
  private def comparisonSuffix[p: P]: P[Expr => Expr] =
    P(comparison ~ concatenation).map { case (op, right) =>
      (left: Expr) => BinaryExpr(left, op, right).setPos(left.pos).asInstanceOf[Expr]
    }

  private def betweenSuffix[p: P]: P[Expr => Expr] =
    P(betweenOp ~ concatenation ~ kw("and") ~ concatenation).map { case (op, lower, upper) =>
      (left: Expr) => BetweenExpr(left, op, lower, upper).setPos(left.pos).asInstanceOf[Expr]
    }

  private def betweenOp[p: P]: P[String] =
    P(
      (kw("not") ~ kw("between") ~ kw("symmetric")).map(_ => "NOT BETWEEN SYMMETRIC")
      | (kw("not") ~ kw("between")).map(_ => "NOT BETWEEN")
      | (kw("between") ~ kw("symmetric")).map(_ => "BETWEEN SYMMETRIC")
      | kw("between").map(_ => "BETWEEN")
    )

  private def isNullSuffix[p: P]: P[Expr => Expr] =
    P(isNull).map { op =>
      (left: Expr) => UnaryExpr(op, left).setPos(left.pos).asInstanceOf[Expr]
    }

  private def inSuffix[p: P]: P[Expr => Expr] =
    P(
      (in ~ "(" ~ query ~ ")").map { case (op, q) =>
        (left: Expr) => InQueryExpr(left, op, q).setPos(left.pos).asInstanceOf[Expr]
      }
      | (in ~ "(" ~ expressions ~ ")").map { case (op, es) =>
          (left: Expr) => InSeqExpr(left, op, es).setPos(left.pos).asInstanceOf[Expr]
        }
    )

  private def isNull[p: P]: P[String] =
    P(
      (kw("is") ~ kw("not") ~ kw("null")).map(_ => "IS NOT NULL")
      | (kw("is") ~ kw("not") ~ kw("true")).map(_ => "IS NOT TRUE")
      | (kw("is") ~ kw("not") ~ kw("false")).map(_ => "IS NOT FALSE")
      | (kw("is") ~ kw("not") ~ kw("unknown")).map(_ => "IS NOT UNKNOWN")
      | (kw("is") ~ kw("null")).map(_ => "IS NULL")
      | (kw("is") ~ kw("true")).map(_ => "IS TRUE")
      | (kw("is") ~ kw("false")).map(_ => "IS FALSE")
      | (kw("is") ~ kw("unknown")).map(_ => "IS UNKNOWN")
    )

  private def in[p: P]: P[String] =
    P(
      (kw("not") ~ kw("in")).map(_ => "NOT IN")
      | kw("in").map(_ => "IN")
    )

  private def comparison[p: P]: P[String] =
    P(
      "<=".! | ">=".! | "<>".!.map(_ => "!=") | "!=".! | "<".! | ">".! | "=".!
      | (kw("not") ~ kw("like")).map(_ => "NOT LIKE")
      | (kw("not") ~ kw("ilike")).map(_ => "NOT ILIKE")
      | kw("like").map(_ => "LIKE")
      | kw("ilike").map(_ => "ILIKE")
    )

  private def booleanLiteral[p: P]: P[Expr] =
    P(Idx ~ (kw("true").map(_ => true) | kw("false").map(_ => false))).map((loc, b) =>
      pos(loc, BooleanExpr(b))
    )

  // ── Star ───────────────────────────────────────────────────────────

  private def qualifiedStar[p: P]: P[Expr] =
    P(Idx ~ identifier ~ "." ~ "*").map((loc, t) => pos(loc, TableStarExpr(t)))

  private def star[p: P]: P[Expr] =
    P(Idx ~ "*").map(loc => pos(loc, StarExpr()))

  // ── JSON literals ──────────────────────────────────────────────────

  private def pair[p: P]: P[(Ident, Expr)] =
    P(identifier ~ ":" ~ (arrayExpression | objectExpression | literal)).map((k, v) => (k, v))

  private def arrayExpression[p: P]: P[Expr] =
    P(Idx ~ "[" ~ (arrayExpression | objectExpression | literal).rep(sep = ",") ~ "]").map((loc, elems) =>
      pos(loc, ArrayExpr(elems))
    )

  private def objectExpression[p: P]: P[Expr] =
    P(Idx ~ "{" ~ pair.rep(sep = ",") ~ "}").map((loc, pairs) =>
      pos(loc, ObjectExpr(pairs))
    )

  private def jsonLiteral[p: P]: P[Expr] = P(arrayExpression | objectExpression)

  // ── CASE expressions ───────────────────────────────────────────────

  private def caseExpression[p: P]: P[CaseExpr] = P(simpleCaseExpression | searchedCaseExpression)

  // CASE expr WHEN v THEN r ... [ELSE e] END
  private def simpleCaseExpression[p: P]: P[CaseExpr] =
    P(kw("case") ~ expression ~ simpleWhen.rep(1) ~ (kw("else") ~ expression).? ~ kw("end")).map { case (expr, whens, els) =>
      val searchedWhens = whens.map { case (value, result) =>
        When(BinaryExpr(expr, "=", value), result)
      }
      CaseExpr(searchedWhens, els)
    }

  // CASE WHEN cond THEN r ... [ELSE e] END
  private def searchedCaseExpression[p: P]: P[CaseExpr] =
    P(kw("case") ~ when.rep(1) ~ (kw("else") ~ expression).? ~ kw("end")).map { case (ws, e) =>
      CaseExpr(ws, e)
    }

  private def simpleWhen[p: P]: P[(Expr, Expr)] =
    P(kw("when") ~ expression ~ kw("then") ~ expression).map((v, r) => (v, r))

  private def when[p: P]: P[When] =
    P(kw("when") ~ expression ~ kw("then") ~ expression).map((cond, e) => When(cond, e))

  // ── SELECT expressions ─────────────────────────────────────────────

  // selectExpression: qualifiedStar | star | expression [AS alias]
  private def selectExpression[p: P]: P[Expr] =
    P(
      qualifiedStar
      | star
      | (expression ~ (kw("as").? ~ identifier).?).map {
          case (e, None) => e
          case (e, Some(a)) => AliasExpr(e, a).setPos(e.pos).asInstanceOf[Expr]
        }
    )

  private def selectExpressions[p: P]: P[Seq[Expr]] = P(selectExpression.rep(1, sep = ","))

  private def expressions[p: P]: P[Seq[Expr]] = P(expression.rep(1, sep = ","))

  // ── FROM / WHERE / GROUP BY / HAVING / ORDER BY / LIMIT / OFFSET ──

  private def fromClause[p: P]: P[Option[Seq[Expr]]] = P((kw("from") ~ sources.rep(1, sep = ",")).?)

  private def whereClause[p: P]: P[Option[Expr]] = P((kw("where") ~ expression).?)

  private def groupByClause[p: P]: P[Option[Seq[Expr]]] = P((kw("group") ~ kw("by") ~ expression.rep(1, sep = ",")).?)

  private def havingClause[p: P]: P[Option[Expr]] = P((kw("having") ~ expression).?)

  private def orderByClause[p: P]: P[Option[Seq[OrderBy]]] = P((kw("order") ~ kw("by") ~ orderByItem.rep(1, sep = ",")).?)

  private def count[p: P]: P[Count] =
    P(Idx ~ expression).map((loc, e) => Count(mkPos(loc), e))

  private def offsetClause[p: P]: P[Option[Count]] = P((kw("offset") ~ count).?)

  private def limitClause[p: P]: P[Option[Count]] = P((kw("limit") ~ count).?)

  // expression [ASC|DESC] [NULLS FIRST|LAST]
  private def orderByItem[p: P]: P[OrderBy] =
    P(expression ~ (kw("asc").map(_ => "ASC") | kw("desc").map(_ => "DESC")).? ~ (kw("nulls") ~ (kw("first").map(_ => "FIRST") | kw("last").map(_ => "LAST"))).?).map {
      case (e, dir, nulls) =>
        val asc = dir match
          case None | Some("ASC") => true
          case _ => false
        val nullsFirst = nulls match
          case Some("FIRST") => true
          case Some("LAST")  => false
          case None          => !asc // SQL standard: ASC → NULLS LAST, DESC → NULLS FIRST
          case _             => !asc
        OrderBy(e, asc, nullsFirst)
    }

  // ── JOIN sources ───────────────────────────────────────────────────

  private def joinType[p: P]: P[String] =
    P(
      kw("inner").map(_ => "INNER")
      | (kw("left") ~ kw("outer").?).map(_ => "LEFT")
      | (kw("right") ~ kw("outer").?).map(_ => "RIGHT")
      | (kw("full") ~ kw("outer").?).map(_ => "FULL")
    )

  private sealed trait JoinOp
  private case class CrossJoin(right: Expr) extends JoinOp
  private case class CondJoin(jt: Option[String], right: Expr, cond: Expr) extends JoinOp

  private def crossJoinSuffix[p: P]: P[JoinOp] =
    P(kw("cross") ~ kw("join") ~ source).map(r => CrossJoin(r))

  // joinType.? ~ kw("join") ~ source ~ kw("on") ~ expression => (Option[String], Expr, Expr)
  private def condJoinSuffix[p: P]: P[JoinOp] =
    P(joinType.? ~ kw("join") ~ source ~ kw("on") ~ expression).map((jt, r, c) => CondJoin(jt, r, c))

  private def joinSuffix[p: P]: P[JoinOp] = P(crossJoinSuffix | condJoinSuffix)

  private def sources[p: P]: P[Expr] =
    P(source ~ joinSuffix.rep).map { case (first, joins) =>
      joins.foldLeft(first) {
        case (left, CrossJoin(right)) => CrossOperator(left, right).setPos(left.pos).asInstanceOf[Expr]
        case (left, CondJoin(Some("LEFT"), right, cond)) => LeftJoinOperator(left, right, cond).setPos(left.pos).asInstanceOf[Expr]
        case (left, CondJoin(Some("RIGHT"), right, cond)) => RightJoinOperator(left, right, cond).setPos(left.pos).asInstanceOf[Expr]
        case (left, CondJoin(Some("FULL"), right, cond)) => FullJoinOperator(left, right, cond).setPos(left.pos).asInstanceOf[Expr]
        case (left, CondJoin(_, right, cond)) => InnerJoinOperator(left, right, cond).setPos(left.pos).asInstanceOf[Expr]
      }
    }

  // [AS] alias [(col1, col2, ...)]
  // With explicit AS, any identifier is allowed. Without AS, exclude join/clause keywords to avoid ambiguity.
  private def aliasIdentifier[p: P]: P[Ident] =
    P(Idx ~ (quotedIdent | aliasIdent)).map((loc, name) => pos(loc, Ident(name)))

  private def aliasSuffix[p: P]: P[(Ident, Option[Seq[Ident]])] =
    P(
      (kw("as") ~ identifier ~ ("(" ~ identifier.rep(1, sep = ",") ~ ")").?)
      | (aliasIdentifier ~ ("(" ~ identifier.rep(1, sep = ",") ~ ")").?)
    )

  private def source[p: P]: P[Expr] = P(lateralSource | baseSource)

  private def lateralSource[p: P]: P[Expr] =
    P(kw("lateral") ~ "(" ~ query ~ ")" ~ aliasSuffix.?).map {
      case (q, None) => LateralExpr(q).setPos(q.pos).asInstanceOf[Expr]
      case (q, Some((a, None))) => AliasOperator(LateralExpr(q).setPos(q.pos).asInstanceOf[Expr], a).setPos(q.pos).asInstanceOf[Expr]
      case (q, Some((a, Some(cols)))) => ColumnAliasOperator(LateralExpr(q).setPos(q.pos).asInstanceOf[Expr], a, cols).setPos(q.pos).asInstanceOf[Expr]
    }

  private def baseSource[p: P]: P[Expr] =
    P(sourceBase ~ aliasSuffix.?).map {
      case (s, None) => s
      case (s, Some((a, None))) => AliasOperator(s, a).setPos(s.pos).asInstanceOf[Expr]
      case (s, Some((a, Some(cols)))) => ColumnAliasOperator(s, a, cols).setPos(s.pos).asInstanceOf[Expr]
    }

  private def sourceBase[p: P]: P[Expr] =
    P(application | table | valuesClause | ("(" ~ query ~ ")"))

  private def table[p: P]: P[Expr] =
    P(Idx ~ ident ~ ("." ~ anyIdentOrQuoted).?).map {
      case (loc, schema, Some(name)) if schema == "information_schema" =>
        pos(loc, InformationSchemaOperator(pos(loc, Ident(name))))
      case (loc, schema, Some(name)) =>
        pos(loc, TableOperator(pos(loc, Ident(s"$schema.$name"))))
      case (loc, name, None) =>
        pos(loc, TableOperator(pos(loc, Ident(name))))
    }

  // ── VALUES clause ──────────────────────────────────────────────────

  private def valuesClause[p: P]: P[Expr] =
    P(kw("values") ~ ("(" ~ expression.rep(1, sep = ",") ~ ")").rep(1, sep = ",")).map(rows => ValuesExpr(rows))

  // ── SELECT core + compound ─────────────────────────────────────────

  // SELECT [DISTINCT] exprs FROM ... WHERE ... GROUP BY ... HAVING ...
  private def selectCore[p: P]: P[Expr] =
    P(selectStmt | valuesClause | ("(" ~ compoundSelect ~ ")"))

  // DISTINCT / DISTINCT ON (expr, ...) / nothing
  private sealed trait DistinctSpec
  private case object DistinctAll extends DistinctSpec
  private case class DistinctOnSpec(keys: Seq[Expr]) extends DistinctSpec

  private def distinctClause[p: P]: P[DistinctSpec] =
    P(
      (kw("distinct") ~ kw("on") ~ "(" ~ expression.rep(1, sep = ",") ~ ")").map(DistinctOnSpec(_))
      | kw("distinct").map(_ => DistinctAll)
    )

  private def selectStmt[p: P]: P[Expr] =
    P(kw("select") ~ distinctClause.? ~ selectExpressions ~ fromClause ~ whereClause ~ groupByClause ~ havingClause).map {
      case (Some(DistinctOnSpec(keys)), p, f, w, g, h) =>
        SQLSelectExpr(p to ArraySeq, f, w, g, h, None, None, None, distinctOn = Some(keys))
      case (Some(DistinctAll), p, f, w, g, h) =>
        SQLSelectExpr(p to ArraySeq, f, w, g, h, None, None, None, distinct = true)
      case (None, p, f, w, g, h) =>
        SQLSelectExpr(p to ArraySeq, f, w, g, h, None, None, None)
    }

  private def intersectSelect[p: P]: P[Expr] =
    P(selectCore ~ (kw("intersect") ~ selectCore).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, r) =>
        SetOperationExpr("INTERSECT", l, r).setPos(l.pos).asInstanceOf[Expr]
      }
    }

  private sealed trait SetOp
  private case class UnionAllOp(right: Expr) extends SetOp
  private case class UnionOp(right: Expr) extends SetOp
  private case class ExceptOp(right: Expr) extends SetOp

  private def compoundSuffix[p: P]: P[SetOp] =
    P(
      (kw("union") ~ kw("all") ~ intersectSelect).map(r => UnionAllOp(r))
      | (kw("union") ~ intersectSelect).map(r => UnionOp(r))
      | (kw("except") ~ intersectSelect).map(r => ExceptOp(r))
    )

  private def compoundSelect[p: P]: P[Expr] =
    P(intersectSelect ~ compoundSuffix.rep).map { case (first, rest) =>
      rest.foldLeft(first) {
        case (l, UnionAllOp(r)) => SetOperationExpr("UNION ALL", l, r).setPos(l.pos).asInstanceOf[Expr]
        case (l, UnionOp(r)) => SetOperationExpr("UNION", l, r).setPos(l.pos).asInstanceOf[Expr]
        case (l, ExceptOp(r)) => SetOperationExpr("EXCEPT", l, r).setPos(l.pos).asInstanceOf[Expr]
      }
    }

  // ── CTE: WITH name [(cols)] AS (query) ────────────────────────────

  private def cteDef[p: P]: P[CTEDef] =
    P(Idx ~ ident ~ ("(" ~ ident.rep(1, sep = ",") ~ ")").? ~ kw("as") ~ "(" ~ query ~ ")").map {
      case (loc, name, cols, q) =>
        val id = Ident(name).setPos(mkPos(loc)).asInstanceOf[Ident]
        CTEDef(id, cols.map(_.map(c => Ident(c).setPos(mkPos(loc)).asInstanceOf[Ident])), q)
    }

  private def withClause[p: P]: P[(Boolean, Seq[CTEDef])] =
    P(kw("with") ~ kw("recursive").!.?.map(_.isDefined) ~ cteDef.rep(1, sep = ","))

  // query = [WITH [RECURSIVE] ...] compoundSelect [ORDER BY ...] [LIMIT ...] [OFFSET ...]
  private def query[p: P]: P[Expr] =
    P(withClause.? ~ compoundSelect ~ orderByClause ~ limitClause ~ offsetClause).map {
      case (None, s: SQLSelectExpr, o, l, of) => s.copy(orderBy = o, limit = l, offset = of)
      case (None, s, None, None, None) => s
      case (None, s, o, l, of) => CompoundQueryExpr(s, o, of, l)
      case (Some((rec, ctes)), s: SQLSelectExpr, o, l, of) =>
        WithExpr(ctes, s.copy(orderBy = o, limit = l, offset = of), rec)
      case (Some((rec, ctes)), s, None, None, None) => WithExpr(ctes, s, rec)
      case (Some((rec, ctes)), s, o, l, of) => WithExpr(ctes, CompoundQueryExpr(s, o, of, l), rec)
    }

  // ── DML: INSERT ────────────────────────────────────────────────────

  private def rowValue[p: P]: P[Expr] = P(kw("default").map(_ => DefaultExpr) | expression)
  private def row[p: P]: P[Seq[Expr]] = P("(" ~ rowValue.rep(1, sep = ",") ~ ")")

  private def set[p: P]: P[UpdateSet] =
    P(identifier ~ "=" ~ expression).map((col, v) => UpdateSet(col, v))

  private def onConflictClause[p: P]: P[OnConflict] =
    P(kw("on") ~ kw("conflict") ~ (doNothing | doUpdate))

  private def doNothing[p: P]: P[OnConflict] =
    P(kw("do") ~ kw("nothing")).map(_ => OnConflictDoNothing)

  // "(" ~ cols ~ ")" ~ DO UPDATE SET assignments => (Seq[Ident], Seq[UpdateSet])
  private def doUpdate[p: P]: P[OnConflict] =
    P("(" ~ identifier.rep(1, sep = ",") ~ ")" ~ kw("do") ~ kw("update") ~ kw("set") ~ set.rep(1, sep = ",")).map {
      (cols, assignments) => OnConflictDoUpdate(cols, assignments)
    }

  private def returningClause[p: P]: P[Seq[Expr]] =
    P(kw("returning") ~ ("*".!.map(_ => Seq(StarExpr(): Expr)) | expression.rep(1, sep = ",")))

  // INSERT INTO table [(cols)] VALUES (row), ... [ON CONFLICT ...] [RETURNING ...]
  // INSERT INTO table [(cols)] query [ON CONFLICT ...] [RETURNING ...]
  private def insertValues[p: P]: P[Command] =
    P(kw("insert") ~ kw("into") ~ tableIdent ~ ("(" ~ identifier.rep(1, sep = ",") ~ ")").? ~ kw("values") ~ row.rep(1, sep = ",") ~ onConflictClause.? ~ returningClause.?).map {
      case (t, cs, rows, oc, ret) => InsertCommand(t, cs, rows, ret, oc)
    }

  private def insertSelect[p: P]: P[Command] =
    P(kw("insert") ~ kw("into") ~ tableIdent ~ ("(" ~ identifier.rep(1, sep = ",") ~ ")").? ~ query ~ onConflictClause.? ~ returningClause.?).map {
      case (t, cs, q, oc, ret) => InsertSelectCommand(t, cs, q, ret, oc)
    }

  private def insert[p: P]: P[Command] = P(insertValues | insertSelect)

  // ── DML: UPDATE ────────────────────────────────────────────────────

  private def update[p: P]: P[Command] =
    P(kw("update") ~ tableIdent ~ kw("set") ~ set.rep(1, sep = ",") ~
      (kw("from") ~ sources.rep(1, sep = ",")).? ~
      (kw("where") ~ expression).? ~
      returningClause.?).map { case (t, ss, f, c, ret) =>
      UpdateCommand(t, ss, f, c, ret)
    }

  // ── DML: DELETE ────────────────────────────────────────────────────

  private def delete[p: P]: P[Command] =
    P(kw("delete") ~ kw("from") ~ tableIdent ~ (kw("where") ~ expression).? ~ returningClause.?).map {
      case (t, c, ret) => DeleteCommand(t, c, ret)
    }

  // ── DML: TRUNCATE ──────────────────────────────────────────────────

  private def truncate[p: P]: P[Command] =
    P(kw("truncate") ~ kw("table").? ~ tableIdent).map(TruncateCommand(_))

  // ── DDL: Constraints ───────────────────────────────────────────────

  private def referentialAction[p: P]: P[ReferentialAction] =
    P(
      kw("cascade").map(_ => ReferentialAction.Cascade)
      | kw("restrict").map(_ => ReferentialAction.Restrict)
      | (kw("set") ~ kw("null")).map(_ => ReferentialAction.SetNull)
      | (kw("no") ~ kw("action")).map(_ => ReferentialAction.NoAction)
    )

  private def onDeleteClause[p: P]: P[ReferentialAction] = P(kw("on") ~ kw("delete") ~ referentialAction)
  private def onUpdateClause[p: P]: P[ReferentialAction] = P(kw("on") ~ kw("update") ~ referentialAction)

  private def uniqueConstraint[p: P]: P[Option[String] => TableConstraint] =
    P(kw("unique") ~ "(" ~ identifier.rep(1, sep = ",") ~ ")").map { cols =>
      (name: Option[String]) => UniqueConstraint(name, cols)
    }

  private def pkConstraint[p: P]: P[Option[String] => TableConstraint] =
    P(kw("primary") ~ kw("key") ~ "(" ~ identifier.rep(1, sep = ",") ~ ")").map { cols =>
      (name: Option[String]) => PrimaryKeyConstraint(name, cols)
    }

  private def fkConstraint[p: P]: P[Option[String] => TableConstraint] =
    P(kw("foreign") ~ kw("key") ~ "(" ~ identifier.rep(1, sep = ",") ~ ")" ~ kw("references") ~ tableIdent ~ "(" ~ identifier.rep(1, sep = ",") ~ ")" ~ onDeleteClause.? ~ onUpdateClause.?).map {
      case (cols, tbl, refCols, onDel, onUpd) =>
        (name: Option[String]) => ForeignKeyConstraint(name, cols, tbl, refCols, onDel.getOrElse(ReferentialAction.NoAction), onUpd.getOrElse(ReferentialAction.NoAction))
    }

  private def checkConstraint[p: P]: P[Option[String] => TableConstraint] =
    P(kw("check") ~ "(" ~ expression ~ ")").map { expr =>
      (name: Option[String]) => CheckConstraint(name, expr)
    }

  private def constraintBody[p: P]: P[Option[String] => TableConstraint] =
    P(uniqueConstraint | pkConstraint | fkConstraint | checkConstraint)

  private def tableConstraint[p: P]: P[TableConstraint] =
    P((kw("constraint") ~ identifier).? ~ constraintBody).map { case (name, constraint) =>
      constraint(name.map(_.name))
    }

  // ── DDL: Column constraints ────────────────────────────────────────

  private sealed trait ColConstraint
  private case object ColPrimaryKey extends ColConstraint
  private case object ColNotNull extends ColConstraint
  private case object ColNull extends ColConstraint
  private case object ColUnique extends ColConstraint
  private case class ColDefault(expr: Expr) extends ColConstraint
  private case class ColReferences(table: Ident, column: Ident, onDel: ReferentialAction, onUpd: ReferentialAction) extends ColConstraint
  private case class ColCheck(expr: Expr) extends ColConstraint
  private case class ColGenerated(expr: Expr) extends ColConstraint

  private def colConstraint[p: P]: P[ColConstraint] =
    P(
      (kw("primary") ~ kw("key")).map(_ => ColPrimaryKey)
      | (kw("not") ~ kw("null")).map(_ => ColNotNull)
      | kw("null").map(_ => ColNull)
      | kw("unique").map(_ => ColUnique)
      | (kw("default") ~ expression).map(ColDefault(_))
      | (kw("generated") ~ kw("always") ~ kw("as") ~ "(" ~ expression ~ ")" ~ kw("stored")).map(ColGenerated(_))
      | colReferences
      | (kw("check") ~ "(" ~ expression ~ ")").map(ColCheck(_))
    )

  // REFERENCES table(column) [ON DELETE ...] [ON UPDATE ...]
  private def colReferences[p: P]: P[ColConstraint] =
    P(kw("references") ~ identifier ~ "(" ~ identifier ~ ")" ~ onDeleteClause.? ~ onUpdateClause.?).map {
      case (table, column, onDel, onUpd) =>
        ColReferences(table, column, onDel.getOrElse(ReferentialAction.NoAction), onUpd.getOrElse(ReferentialAction.NoAction))
    }

  // identifier ~ typ ~ colConstraint.rep => (Ident, Either[Type,Ident], Seq[ColConstraint])
  private def columnDesc[p: P]: P[ColumnDesc] =
    P(identifier ~ typ ~ colConstraint.rep).map { case (name, t, constraints) =>
      var primaryKey = false
      var required = false
      var unique = false
      var default: Option[Expr] = None
      var references: Option[(Ident, Ident, ReferentialAction, ReferentialAction)] = None
      var check: Option[Expr] = None
      var generated: Option[Expr] = None

      for c <- constraints do
        c match
          case ColPrimaryKey =>
            if primaryKey then throw SchemaException(name.pos, s"duplicate PRIMARY KEY constraint on column '${name.name}'")
            primaryKey = true
          case ColNotNull =>
            if required then throw SchemaException(name.pos, s"duplicate NOT NULL constraint on column '${name.name}'")
            required = true
          case ColNull => ()
          case ColUnique =>
            if unique then throw SchemaException(name.pos, s"duplicate UNIQUE constraint on column '${name.name}'")
            unique = true
          case ColDefault(expr) =>
            if default.isDefined then throw SchemaException(name.pos, s"duplicate DEFAULT clause on column '${name.name}'")
            if generated.isDefined then throw SchemaException(name.pos, s"a generated column cannot have a DEFAULT on column '${name.name}'")
            default = Some(expr)
          case ColReferences(tbl, col, onDel, onUpd) =>
            if references.isDefined then throw SchemaException(name.pos, s"duplicate REFERENCES constraint on column '${name.name}'")
            references = Some((tbl, col, onDel, onUpd))
          case ColCheck(expr) =>
            if check.isDefined then throw SchemaException(name.pos, s"duplicate CHECK constraint on column '${name.name}'")
            check = Some(expr)
          case ColGenerated(expr) =>
            if generated.isDefined then throw SchemaException(name.pos, s"duplicate GENERATED ALWAYS AS clause on column '${name.name}'")
            if default.isDefined then throw SchemaException(name.pos, s"a generated column cannot have a DEFAULT on column '${name.name}'")
            generated = Some(expr)

      ColumnDesc(name, t, required, unique, default, references, check, primaryKey, generated)
    }

  // ── DDL: CREATE/DROP VIEW ────────────────────────────────────────────

  private def createView[p: P]: P[Command] =
    P(kw("create") ~ (kw("or") ~ kw("replace")).!.? ~ kw("view") ~ identifier ~ kw("as") ~ query).map {
      case (orReplace, name, q) => CreateViewCommand(name, q, orReplace.isDefined)
    }

  private def dropView[p: P]: P[Command] =
    P(
      (kw("drop") ~ kw("view") ~ kw("if") ~ kw("exists") ~ identifier).map(name => DropViewCommand(name, true))
      | (kw("drop") ~ kw("view") ~ identifier).map(name => DropViewCommand(name, false))
    )

  // ── DDL: CREATE TABLE ──────────────────────────────────────────────

  private def tableItem[p: P]: P[ColumnDesc | TableConstraint] =
    P(columnDesc.map(_.asInstanceOf[ColumnDesc | TableConstraint]) | tableConstraint.map(_.asInstanceOf[ColumnDesc | TableConstraint]))

  private def createTable[p: P]: P[Command] =
    P(kw("create") ~ (kw("temp") | kw("temporary")).!.? ~ kw("table") ~ (kw("if") ~ kw("not") ~ kw("exists")).!.? ~ tableIdent ~ "(" ~ tableItem.rep(1, sep = ",") ~ ")").map {
      case (temp, ine, t, items) =>
        val columns = items.collect { case c: ColumnDesc => c }
        val constraints = items.collect { case c: TableConstraint => c }
        CreateTableCommand(t, columns, constraints, ine.isDefined, temp.isDefined)
    }

  // ── DDL: DROP TABLE ────────────────────────────────────────────────

  private def cascadeRestrict[p: P]: P[Boolean] =
    P(kw("cascade").!.map(_ => true) | kw("restrict").!.map(_ => false))

  private def dropTable[p: P]: P[Command] =
    P(
      (kw("drop") ~ kw("table") ~ kw("if") ~ kw("exists") ~ tableIdent ~ cascadeRestrict.?).map {
        case (t, cascade) => DropTableCommand(t, true, cascade.getOrElse(false))
      }
      | (kw("drop") ~ kw("table") ~ tableIdent ~ cascadeRestrict.?).map {
          case (t, cascade) => DropTableCommand(t, false, cascade.getOrElse(false))
        }
    )

  // ── DDL: CREATE/DROP INDEX ─────────────────────────────────────────

  // CREATE [UNIQUE] INDEX name ON table [USING method] (cols)
  private def createIndex[p: P]: P[Command] =
    P(kw("create") ~ kw("unique").!.? ~ kw("index") ~ identifier ~ kw("on") ~ tableIdent ~ (kw("using") ~ identifier).? ~ "(" ~ identifier.rep(1, sep = ",") ~ ")").map {
      case (u, name, table, method, cols) =>
        method.foreach { m =>
          if m.name.toLowerCase != "btree" then
            throw SchemaException(m.pos, s"index method '${m.name}' is not supported, only 'btree' is available")
        }
        CreateIndexCommand(name, table, cols, u.isDefined)
    }

  private def dropIndex[p: P]: P[Command] =
    P(
      (kw("drop") ~ kw("index") ~ kw("if") ~ kw("exists") ~ identifier).map(name => DropIndexCommand(name, true))
      | (kw("drop") ~ kw("index") ~ identifier).map(name => DropIndexCommand(name, false))
    )

  // ── DDL: CREATE/DROP TYPE ──────────────────────────────────────────

  // CREATE TYPE name AS ENUM ('a', 'b', ...)
  private def createType[p: P]: P[Command] =
    P(kw("create") ~ kw("type") ~ identifier ~ kw("as") ~ kw("enum") ~ "(" ~ stringLit.rep(1, sep = ",") ~ ")").map {
      (t, ls) => CreateEnumCommand(t, ls)
    }

  private def dropType[p: P]: P[Command] =
    P(
      (kw("drop") ~ kw("type") ~ kw("if") ~ kw("exists") ~ identifier ~ (kw("cascade").!.map(_ => true) | kw("restrict").!.map(_ => false)).?).map {
        case (name, cascade) => DropTypeCommand(name, true, cascade.getOrElse(false))
      }
      | (kw("drop") ~ kw("type") ~ identifier ~ (kw("cascade").!.map(_ => true) | kw("restrict").!.map(_ => false)).?).map {
          case (name, cascade) => DropTypeCommand(name, false, cascade.getOrElse(false))
        }
    )

  // ── DDL: CREATE/DROP SEQUENCE ─────────────────────────────────────

  private sealed trait SeqOption
  private case class SeqIncrement(v: Long) extends SeqOption
  private case class SeqMinValue(v: Option[Long]) extends SeqOption
  private case class SeqMaxValue(v: Option[Long]) extends SeqOption
  private case class SeqStart(v: Long) extends SeqOption
  private case class SeqCycle(v: Boolean) extends SeqOption

  private def seqOption[p: P]: P[SeqOption] =
    P(
      (kw("increment") ~ kw("by").? ~ signedLongLit).map(n => SeqIncrement(n))
      | (kw("minvalue") ~ signedLongLit).map(n => SeqMinValue(Some(n)))
      | (kw("no") ~ kw("minvalue")).map(_ => SeqMinValue(None))
      | (kw("maxvalue") ~ signedLongLit).map(n => SeqMaxValue(Some(n)))
      | (kw("no") ~ kw("maxvalue")).map(_ => SeqMaxValue(None))
      | (kw("start") ~ kw("with").? ~ signedLongLit).map(n => SeqStart(n))
      | kw("cycle").map(_ => SeqCycle(true))
      | (kw("no") ~ kw("cycle")).map(_ => SeqCycle(false))
    )

  private def signedLongLit[p: P]: P[Long] =
    P(("-".!.? ~ CharIn("0-9").rep(1).!).map { case (neg, digits) =>
      val v = digits.toLong
      if neg.isDefined then -v else v
    })

  private def createSequence[p: P]: P[Command] =
    P(kw("create") ~ kw("sequence") ~ (kw("if") ~ kw("not") ~ kw("exists")).!.? ~ identifier ~ seqOption.rep).map {
      case (ine, name, opts) =>
        var increment = 1L
        var minValue: Option[Long] = None
        var maxValue: Option[Long] = None
        var startValue: Option[Long] = None
        var cycle = false
        for opt <- opts do opt match
          case SeqIncrement(v) => increment = v
          case SeqMinValue(v)  => minValue = v
          case SeqMaxValue(v)  => maxValue = v
          case SeqStart(v)     => startValue = Some(v)
          case SeqCycle(v)     => cycle = v
        CreateSequenceCommand(name, increment, minValue, maxValue, startValue, cycle, ine.isDefined)
    }

  private def dropSequence[p: P]: P[Command] =
    P(
      (kw("drop") ~ kw("sequence") ~ kw("if") ~ kw("exists") ~ identifier).map(name => DropSequenceCommand(name, true))
      | (kw("drop") ~ kw("sequence") ~ identifier).map(name => DropSequenceCommand(name, false))
    )

  // ── DDL: ALTER TABLE ───────────────────────────────────────────────

  private def alterTable[p: P]: P[Command] =
    P(kw("alter") ~ kw("table") ~ tableIdent ~ tableAlteration).map((t, a) => AlterTableCommand(t, a))

  private def tableAlteration[p: P]: P[TableAlteration] =
    P(
      renameTableTo
      | renameColumn
      | addColumn
      | addConstraint
      | addLegacyForeignKey
      | dropColumn
      | dropConstraint
      | alterColumn
    )

  private def renameTableTo[p: P]: P[TableAlteration] =
    P(kw("rename") ~ kw("to") ~ identifier).map(RenameTableAlteration(_))

  private def renameColumn[p: P]: P[TableAlteration] =
    P(kw("rename") ~ kw("column").? ~ identifier ~ kw("to") ~ identifier).map((old, newN) =>
      RenameColumnTableAlteration(old, newN)
    )

  private def addColumn[p: P]: P[TableAlteration] =
    P(kw("add") ~ kw("column").? ~ columnDesc).map(AddColumnTableAlteration(_))

  private def addConstraint[p: P]: P[TableAlteration] =
    P(kw("add") ~ (kw("constraint") ~ identifier).? ~ constraintBody).map { case (name, constraint) =>
      AddConstraintTableAlteration(constraint(name.map(_.name)))
    }

  private def addLegacyForeignKey[p: P]: P[TableAlteration] =
    P(kw("add") ~ kw("foreign") ~ kw("key") ~ "(" ~ identifier ~ ")" ~ kw("references") ~ identifier).map {
      (fk, ref) => AddForeignKeyTableAlteration(fk, ref)
    }

  private def dropColumn[p: P]: P[TableAlteration] =
    P(kw("drop") ~ kw("column").? ~ !kw("constraint") ~ identifier ~ (kw("cascade") | kw("restrict")).?).map {
      col => DropColumnTableAlteration(col)
    }

  private def dropConstraint[p: P]: P[TableAlteration] =
    P(kw("drop") ~ kw("constraint") ~ identifier ~ (kw("cascade") | kw("restrict")).?).map {
      name => DropConstraintTableAlteration(name)
    }

  private def alterColumn[p: P]: P[TableAlteration] =
    P(kw("alter") ~ kw("column").? ~ identifier ~ columnModification).map((col, mod) =>
      AlterColumnTableAlteration(col, mod)
    )

  private def columnModification[p: P]: P[ColumnModification] =
    P(
      (kw("type") ~ typ).map(SetDataTypeColumnModification(_))
      | (kw("set") ~ kw("not") ~ kw("null")).map(_ => SetNotNullColumnModification())
      | (kw("drop") ~ kw("not") ~ kw("null")).map(_ => DropNotNullColumnModification())
      | (kw("set") ~ kw("default") ~ expression).map(SetDefaultColumnModification(_))
      | (kw("drop") ~ kw("default")).map(_ => DropDefaultColumnModification())
    )

  // ── PREPARE / EXECUTE / DEALLOCATE ─────────────────────────────────

  private def prepare[p: P]: P[Command] =
    P(kw("prepare") ~ identifier ~ kw("as") ~ command).map((name, cmd) =>
      PrepareCommand(name, Seq(cmd))
    )

  private def executeCmd[p: P]: P[Command] =
    P(kw("execute") ~ identifier ~ ("(" ~ expression.rep(1, sep = ",") ~ ")").?).map { case (name, params) =>
      ExecuteCommand(name, params.getOrElse(Nil))
    }

  private def deallocate[p: P]: P[Command] =
    P(kw("deallocate") ~ kw("prepare").? ~ identifier).map(DeallocateCommand(_))

  // ── DML: COPY ────────────────────────────────────────────────────

  private def copyOption[p: P]: P[Either[Unit, String]] =
    P(kw("header").map(_ => Left(())) | (kw("delimiter") ~ stringLit).map(Right(_)))

  private def copyOptions[p: P]: P[(Boolean, Char)] =
    P(kw("with") ~ "(" ~ copyOption.rep(1, sep = ",") ~ ")").map { opts =>
      var header = false
      var delimiter = ','
      for opt <- opts do
        opt match
          case Left(()) => header = true
          case Right(s) =>
            if s.length != 1 then sys.error(s"COPY delimiter must be a single character, got '$s'")
            delimiter = s.charAt(0)
      (header, delimiter)
    }

  private def copyFrom[p: P]: P[Command] =
    P(kw("copy") ~ tableIdent ~ ("(" ~ identifier.rep(1, sep = ",") ~ ")").? ~ kw("from") ~ stringLit ~ copyOptions.?)
      .map { case (table, cols, file, opts) =>
        val (header, delim) = opts.getOrElse((false, ','))
        CopyFromCommand(table, cols, file, header, delim)
      }

  private def copyToSource[p: P]: P[Either[Ident, Expr]] =
    P(("(" ~ query ~ ")").map(Right(_)) | identifier.map(Left(_)))

  private def copyTo[p: P]: P[Command] =
    P(kw("copy") ~ copyToSource ~ kw("to") ~ stringLit ~ copyOptions.?)
      .map { case (source, file, opts) =>
        val (header, delim) = opts.getOrElse((false, ','))
        CopyToCommand(source, file, header, delim)
      }

  private def copyCmd[p: P]: P[Command] = P(copyFrom | copyTo)

  // ── Transaction commands ───────────────────────────────────────────

  private def beginCmd[p: P]: P[Command] = P(kw("begin") ~ kw("transaction").?).map(_ => BeginCommand)
  private def commitCmd[p: P]: P[Command] = P(kw("commit") ~ kw("transaction").?).map(_ => CommitCommand)
  private def rollbackCmd[p: P]: P[Command] = P(kw("rollback") ~ kw("transaction").?).map(_ => RollbackCommand)

  // ── EXPLAIN ────────────────────────────────────────────────────────

  private def explain[p: P]: P[Command] = P(kw("explain") ~ command).map(ExplainCommand(_))

  // ── SHOW commands ─────────────────────────────────────────────────

  private def showTables[p: P]: P[Command] =
    P(kw("show") ~ kw("tables")).map(_ => ShowTablesCommand)
  private def showViews[p: P]: P[Command] =
    P(kw("show") ~ kw("views")).map(_ => ShowViewsCommand)
  private def showColumns[p: P]: P[Command] =
    P(kw("show") ~ kw("columns") ~ kw("from").? ~ tableIdent).map(ShowColumnsCommand(_))
  private def showPrimaryKey[p: P]: P[Command] =
    P(kw("show") ~ kw("primary") ~ kw("key") ~ tableIdent).map(ShowPrimaryKeyCommand(_))
  private def showForeignKeys[p: P]: P[Command] =
    P(kw("show") ~ kw("foreign") ~ kw("keys") ~ tableIdent).map(ShowForeignKeysCommand(_))
  private def showIndexes[p: P]: P[Command] =
    P(kw("show") ~ kw("indexes") ~ tableIdent).map(ShowIndexesCommand(_))
  private def showSequences[p: P]: P[Command] =
    P(kw("show") ~ kw("sequences")).map(_ => ShowSequencesCommand)
  private def showAllIndexes[p: P]: P[Command] =
    P(kw("show") ~ kw("indexes")).map(_ => ShowAllIndexesCommand)
  private def showCmd[p: P]: P[Command] = P(showTables | showViews | showSequences | showPrimaryKey | showForeignKeys | showIndexes | showAllIndexes | showColumns)

  // ── Top-level command ──────────────────────────────────────────────

  private def commandTxn[p: P]: P[Command] =
    P(explain | showCmd | beginCmd | commitCmd | rollbackCmd | prepare | executeCmd | deallocate)

  private def createSchema[p: P]: P[Command] =
    P(kw("create") ~ kw("schema") ~ (kw("if") ~ kw("not") ~ kw("exists")).!.? ~ identifier).map {
      case (ine, name) => CreateSchemaCommand(name, ine.isDefined)
    }

  private def dollarQuote[p: P]: P[Unit] = {
    import NoWhitespace._
    P("$$")
  }

  private def doBlock[p: P]: P[Command] =
    P(kw("do") ~ dollarQuote ~ kw("begin") ~ command ~ ";" ~ kw("exception") ~ kw("when") ~ ident ~ kw("then") ~ kw("null") ~ ";" ~ kw("end") ~ dollarQuote)
      .map { case (cmd, _) => DoBlockCommand(cmd) }

  private def commandDDL[p: P]: P[Command] =
    P(createSchema | createSequence | createView | createTable | createIndex | createType | dropSequence | dropView | dropTable | dropIndex | dropType | alterTable | doBlock)

  private def commandDML[p: P]: P[Command] =
    P(copyCmd | insert | update | delete | truncate | query.map(QueryCommand(_)))

  private def command[p: P]: P[Command] = P(commandTxn | commandDML | commandDDL)

  private def commands[p: P]: P[Seq[Command]] =
    P(Pass ~ ";".rep ~ (command ~ (";".rep(1) ~ command).rep ~ ";".rep).? ~ End)
      .map(_.fold(Seq.empty[Command]) { case (first, rest) => first +: rest })

  // ── Public API ─────────────────────────────────────────────────────

  private def run[T](input: String, parser: P[?] => P[T]): T =
    fastparse.parse(input, parser) match
      case Parsed.Success(result, _) => result
      case f: Parsed.Failure =>
        val loc = Loc(new IndexedParserInput(input), f.index)
        throw ParseException(mkPos(loc), f.trace().longMsg)

  def parseCommands(input: String): Seq[Command] = run(input, { implicit p => commands })

  def parseCommand(input: String): Command = run(input, { implicit p => P(command ~ End) })

  def parseQuery(input: String): Expr = run(input, { implicit p => P(query ~ End) })

  def parseBooleanExpression(input: String): Expr = run(input, { implicit p => P(expression ~ End) })
