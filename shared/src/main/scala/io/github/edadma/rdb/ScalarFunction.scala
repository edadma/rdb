package io.github.edadma.rdb

import scala.math.*

case class ScalarFunction(name: String, func: PartialFunction[Seq[Value], Value], typ: Type)

val scalarFunction: Map[String, ScalarFunction] =
  List(
    ScalarFunction("ABS", { case Seq(NumberValue(t, n)) => NumberValue(abs(n.doubleValue)) }, NumberType),
    ScalarFunction("TABLE", { case Seq(TableValue(d, _)) => ArrayValue(d map (r => ArrayValue(r.data))) }, ArrayType),
    ScalarFunction("TYPEOF", { case Seq(v) => TextValue(v.vtyp.name) }, TextType),
    // Text functions
    ScalarFunction("LOWER", { case Seq(TextValue(t)) => TextValue(t.toLowerCase) }, TextType),
    ScalarFunction("UPPER", { case Seq(TextValue(t)) => TextValue(t.toUpperCase) }, TextType),
    ScalarFunction("LENGTH", { case Seq(TextValue(t)) => NumberValue(t.length) }, NumberType),
    ScalarFunction("CHAR_LENGTH", { case Seq(TextValue(t)) => NumberValue(t.length) }, NumberType),
    ScalarFunction("TRIM", { case Seq(TextValue(t)) => TextValue(t.trim) }, TextType),
    ScalarFunction("LTRIM", { case Seq(TextValue(t)) => TextValue(t.replaceAll("^\\s+", "")) }, TextType),
    ScalarFunction("RTRIM", { case Seq(TextValue(t)) => TextValue(t.replaceAll("\\s+$", "")) }, TextType),
    ScalarFunction(
      "SUBSTRING",
      {
        // SUBSTRING(text, start)
        case Seq(TextValue(s), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0    = math.min(math.max(0, start - 1), s.length)
          TextValue(s.substring(i0))
        // SUBSTRING(text, start, length)
        case Seq(TextValue(s), NumberValue(_, startNum), NumberValue(_, lenNum)) =>
          val start = math.max(1, startNum.intValue)
          val len   = math.max(0, lenNum.intValue)
          val i0    = math.min(math.max(0, start - 1), s.length)
          val i1    = math.min(i0 + len, s.length)
          TextValue(s.substring(i0, i1))
      },
      TextType,
    ),
    ScalarFunction(
      "LEFT",
      { case Seq(TextValue(s), NumberValue(_, nNum)) =>
        val n  = math.max(0, nNum.intValue)
        val n2 = math.min(n, s.length)
        TextValue(s.substring(0, n2))
      },
      TextType,
    ),
    ScalarFunction(
      "RIGHT",
      { case Seq(TextValue(s), NumberValue(_, nNum)) =>
        val n  = math.max(0, nNum.intValue)
        val n2 = math.min(n, s.length)
        TextValue(s.substring(s.length - n2))
      },
      TextType,
    ),
    ScalarFunction(
      "REPLACE",
      { case Seq(TextValue(s), TextValue(search), TextValue(repl)) => TextValue(s.replace(search, repl)) },
      TextType,
    ),
    ScalarFunction(
      "CONCAT",
      { case Seq(TextValue(a), TextValue(b)) => TextValue(a + b) },
      TextType,
    ),
    ScalarFunction(
      "REPEAT",
      { case Seq(TextValue(s), NumberValue(_, nNum)) => TextValue(s * math.max(0, nNum.intValue)) },
      TextType,
    ),
    ScalarFunction(
      "POSITION",
      {
        case Seq(TextValue(substr), TextValue(s)) =>
          val idx = s.indexOf(substr)
          NumberValue(if idx < 0 then 0 else idx + 1) // 1-based; 0 when not found
      },
      NumberType,
    ),
    // Numeric functions
    ScalarFunction("CEIL", { case Seq(NumberValue(_, n)) => NumberValue(math.ceil(n.doubleValue)) }, NumberType),
    ScalarFunction("CEILING", { case Seq(NumberValue(_, n)) => NumberValue(math.ceil(n.doubleValue)) }, NumberType),
    ScalarFunction("FLOOR", { case Seq(NumberValue(_, n)) => NumberValue(math.floor(n.doubleValue)) }, NumberType),
    ScalarFunction(
      "ROUND",
      {
        // ROUND(x)
        case Seq(NumberValue(_, n)) => NumberValue(math.rint(n.doubleValue))
        // ROUND(x, digits)
        case Seq(NumberValue(_, n), NumberValue(_, d)) =>
          val k   = d.intValue
          val m   = math.pow(10.0, k.toDouble)
          val res = if k >= 0 then math.rint(n.doubleValue * m) / m else math.rint(n.doubleValue / m) * m
          NumberValue(res)
      },
      NumberType,
    ),
    ScalarFunction(
      "TRUNC",
      {
        // TRUNC(x)
        case Seq(NumberValue(_, n)) =>
          NumberValue(if n.doubleValue >= 0 then math.floor(n.doubleValue) else math.ceil(n.doubleValue))
        // TRUNC(x, digits)
        case Seq(NumberValue(_, n), NumberValue(_, d)) =>
          val k = d.intValue
          if k >= 0 then
            val m = math.pow(10.0, k.toDouble)
            NumberValue((math.signum(n.doubleValue) * math.floor(math.abs(n.doubleValue) * m)) / m)
          else
            val m = math.pow(10.0, -k.toDouble)
            NumberValue(math.signum(n.doubleValue) * math.floor(math.abs(n.doubleValue) / m) * m)
      },
      NumberType,
    ),
    ScalarFunction("SIGN", { case Seq(NumberValue(_, n)) => NumberValue(math.signum(n.doubleValue)) }, NumberType),
    ScalarFunction(
      "MOD",
      { case Seq(NumberValue(_, a), NumberValue(_, b)) => NumberValue(a.doubleValue % b.doubleValue) },
      NumberType,
    ),
    ScalarFunction(
      "POWER",
      { case Seq(NumberValue(_, x), NumberValue(_, y)) => NumberValue(math.pow(x.doubleValue, y.doubleValue)) },
      NumberType,
    ),
    ScalarFunction("SQRT", { case Seq(NumberValue(_, n)) => NumberValue(math.sqrt(n.doubleValue)) }, NumberType),
    // Arrays
    ScalarFunction("ARRAY_LENGTH", { case Seq(ArrayValue(elems)) => NumberValue(elems.length) }, NumberType),
    ScalarFunction(
      "ARRAY_SLICE",
      {
        // ARRAY_SLICE(arr, start)
        case Seq(ArrayValue(elems), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0    = math.min(math.max(0, start - 1), elems.length)
          ArrayValue(elems.slice(i0, elems.length))
        // ARRAY_SLICE(arr, start, end)
        case Seq(ArrayValue(elems), NumberValue(_, startNum), NumberValue(_, endNum)) =>
          val start = math.max(1, startNum.intValue)
          val end   = math.max(1, endNum.intValue)
          val i0    = math.min(math.max(0, start - 1), elems.length)
          val i1    = math.min(math.max(i0, end), elems.length)
          ArrayValue(elems.slice(i0, i1))
      },
      ArrayType,
    ),
    ScalarFunction(
      "ARRAY_APPEND",
      { case Seq(ArrayValue(elems), v) => ArrayValue(elems :+ v) },
      ArrayType,
    ),
    ScalarFunction(
      "ARRAY_PREPEND",
      { case Seq(v, ArrayValue(elems)) => ArrayValue(v +: elems) },
      ArrayType,
    ),
    ScalarFunction(
      "ARRAY_CONCAT",
      { case Seq(ArrayValue(a), ArrayValue(b)) => ArrayValue(a ++ b) },
      ArrayType,
    ),
    // Padding
    ScalarFunction(
      "LPAD",
      {
        // LPAD(text, len)
        case Seq(TextValue(s), NumberValue(_, lenNum)) =>
          val len     = math.max(0, lenNum.intValue)
          val needed  = math.max(0, len - s.length)
          val padStr  = " "
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val left    = (padStr * repeats).take(needed)
          TextValue((left + s).take(len))
        // LPAD(text, len, pad)
        case Seq(TextValue(s), NumberValue(_, lenNum), TextValue(padStr0)) =>
          val len     = math.max(0, lenNum.intValue)
          val padStr  = if padStr0.isEmpty then " " else padStr0
          val needed  = math.max(0, len - s.length)
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val left    = (padStr * repeats).take(needed)
          TextValue((left + s).take(len))
      },
      TextType,
    ),
    ScalarFunction(
      "RPAD",
      {
        // RPAD(text, len)
        case Seq(TextValue(s), NumberValue(_, lenNum)) =>
          val len     = math.max(0, lenNum.intValue)
          val needed  = math.max(0, len - s.length)
          val padStr  = " "
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val right   = (padStr * repeats).take(needed)
          TextValue((s + right).take(len))
        // RPAD(text, len, pad)
        case Seq(TextValue(s), NumberValue(_, lenNum), TextValue(padStr0)) =>
          val len     = math.max(0, lenNum.intValue)
          val padStr  = if padStr0.isEmpty then " " else padStr0
          val needed  = math.max(0, len - s.length)
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val right   = (padStr * repeats).take(needed)
          TextValue((s + right).take(len))
      },
      TextType,
    ),
    // Aliases and extra math
    ScalarFunction(
      "SUBSTR",
      {
        case Seq(TextValue(s), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0    = math.min(math.max(0, start - 1), s.length)
          TextValue(s.substring(i0))
        case Seq(TextValue(s), NumberValue(_, startNum), NumberValue(_, lenNum)) =>
          val start = math.max(1, startNum.intValue)
          val len   = math.max(0, lenNum.intValue)
          val i0    = math.min(math.max(0, start - 1), s.length)
          val i1    = math.min(i0 + len, s.length)
          TextValue(s.substring(i0, i1))
      },
      TextType,
    ),
    ScalarFunction("EXP", { case Seq(NumberValue(_, n)) => NumberValue(math.exp(n.doubleValue)) }, NumberType),
    ScalarFunction("LN", { case Seq(NumberValue(_, n)) => NumberValue(math.log(n.doubleValue)) }, NumberType),
    ScalarFunction("LOG10", { case Seq(NumberValue(_, n)) => NumberValue(math.log10(n.doubleValue)) }, NumberType),
    ScalarFunction("RANDOM", { case Seq() => NumberValue(math.random) }, NumberType),
  ).map(f => f.name -> f).toMap
