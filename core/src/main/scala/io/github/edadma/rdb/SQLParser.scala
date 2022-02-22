package io.github.edadma.rdb

import fastparse._

object SQLParser:

  private val delimiters = "[](){}`'\","

  val keywords = Set(
    "and",
    "break",
    "class",
    "continue",
    "def",
    "div",
    "do",
    "else",
    "elsif",
    "extends",
    "false",
    "for",
    "if",
    "match",
    "mod",
    "not",
    "null",
    "or",
    "then",
    "true",
    "val",
    "var",
    "while",
    "with"
  )

  def ident[_: P]: P[Ident] =
    P(
      (Index ~ (CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").repX).!)
        .filter(s => !keywords.contains(s._2))
        .map(Ident.tupled)
    )

  def leftInfix(tree: (Int, ExprAST, Seq[(String, Int, ExprAST)])): ExprAST = {
    val (lpos, base, ops) = tree

    val (_, res) =
      ops.foldLeft((lpos, base)) { case ((lp, left), (op, rp, right)) =>
        (lp, InfixExpr(lp, left, op, rp, right))
      }

    res
  }

  def kw[_: P](s: String): P[Unit] = P(s ~~ !CharPred(_.isLetterOrDigit))

  def sym[_: P, T](p: => P[T]): P[T] =
    P(p ~~ !CharPred(c => !c.isLetterOrDigit && !delimiters.contains(c) && !c.isWhitespace))

    def condition[_: P]: P[ExprAST] = P(disjunctive)

    def disjunctive[_: P]: P[ExprAST] = P(Index ~ conjunctive ~ (kw("or").! ~ Index ~ conjunctive).rep).map(leftInfix)

    def conjunctive[_: P]: P[ExprAST] = P(Index ~ not ~ (kw("and").! ~ Index ~ not).rep).map(leftInfix)

    def not[_: P]: P[ExprAST] =
      P(
        (kw("not").! ~ Index ~ not).map(PrefixExpr.tupled) |
          comparitive
      )

    def comparitive[_: P]: P[ExprAST] =
      P(
        (Index ~ range ~ (sym(StringIn("<=", ">=", "!=", "==", "<", ">", "div")).! ~ Index ~ range)
          .map(Predicate.tupled)
          .rep)
          .map {
            case (_, left, Nil)     => left
            case (pos, left, right) => CompareExpr(pos, left, right)
          }
      )

    def range[_: P]: P[ExprAST] =
      P((Index ~ or ~ (sym(StringIn("..")).! ~ Index ~ or).?).map {
        case (_, expr, None)                       => expr
        case (lpos, left, Some((op, rpos, right))) => InfixExpr(lpos, left, op, rpos, right)
      })

    def or[_: P]: P[ExprAST] = P(Index ~ and ~ (sym("|").! ~ Index ~ and).rep).map(leftInfix)

    def and[_: P]: P[ExprAST] = P(Index ~ invert ~ (sym("&").! ~ Index ~ invert).rep).map(leftInfix)

    def invert[_: P]: P[ExprAST] = P((sym("~").! ~ Index ~ invert).map(PrefixExpr.tupled) | shift)

    def shift[_: P]: P[ExprAST] =
      P((Index ~ additive ~ (sym(StringIn("<<", ">>", ">>>")).! ~ Index ~ additive).rep).map(leftInfix))

    def additive[_: P]: P[ExprAST] =
      P((Index ~ multiplicative ~ (sym(StringIn("+", "-")).! ~ Index ~ multiplicative).rep).map(leftInfix))

    def multiplicative[_: P]: P[ExprAST] =
      P((Index ~ power ~ (sym(StringIn("*", "/", "//", "\\")).! ~ Index ~ power).rep).map(leftInfix))

    def power[_: P]: P[ExprAST] =
      P((Index ~ negative ~ (sym("^") ~ Index ~ power).?).map {
        case (_, left, None)                   => left
        case (lpos, left, Some((rpos, right))) => InfixExpr(lpos, left, "^", rpos, right)
      })

    def negative[_: P]: P[ExprAST] = P((sym("-").! ~ Index ~ negative).map(PrefixExpr.tupled) | incdec)

    def incdec[_: P]: P[ExprAST] =
      P(
        (sym(StringIn("++", "--")).! ~ Index ~ applicative).map(PrefixExpr.tupled) |
          (Index ~ applicative ~ sym(StringIn("++", "--")).!.?).map {
            case (_, expr, None)       => expr
            case (pos, expr, Some(op)) => PostfixExpr(pos, expr, op)
          }
      )

    def applicative[_: P]: P[ExprAST] =
      P(
        (Index ~ primary ~ (("(" ~ (Index ~ expression).map(Arg.tupled).rep(sep = ",").map(Args) ~ ")") |
          ("." ~ ident).map(Dot)).rep).map {
          case (_, expr, Nil)   => expr
          case (pos, expr, ops) => ApplyExpr(pos, expr, ops)
        }
      )

    def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))
    def exponent[_: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
    def fractional[_: P]: P[Unit] = P("." ~ digits)
    def integral[_: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)

    def hexDigit[_: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
    def unicodeEscape[_: P]: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
    def escape[_: P]: P[Unit] = P("\\" ~~ (CharIn("'\"/\\\\bfnrt") | unicodeEscape))
    def strChars[_: P](delim: Char): P[Unit] = P(CharsWhile(c => c != delim && c != '\\'))

    def primary[_: P]: P[ExprAST] =
      P(
        (kw("true") | kw("false")).!.map(BooleanExpr) |
          ident.map(SymExpr) |
          (CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(NumberExpr) |
          kw("null").map(_ => NullExpr) |
          P("()").map(_ => NullExpr) |
          ("`" ~~ interpolator.repX ~~ "`").map(InterpolatedStringExpr) |
          ("'" ~~ (strChars('\'') | escape).repX.! ~~ "'").map(StringExpr) |
          ("\"" ~~ (strChars('"') | escape).repX.! ~~ "\"").map(StringExpr) |
          "{" ~ (expression ~ ":" ~ Index ~ expression).map(MapEntry.tupled).rep(sep = ",").map(MapExpr) ~ "}" |
          "[" ~ expression.rep(sep = ",").map(SeqExpr) ~ "]" |
          "(" ~ expression ~ ")"
      )
