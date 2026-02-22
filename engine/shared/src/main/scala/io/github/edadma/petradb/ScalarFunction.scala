package io.github.edadma.petradb

import scala.math.*

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

case class ScalarFunction(name: String, func: PartialFunction[Seq[Value], Value], typ: Type)

val scalarFunction: Map[String, ScalarFunction] =
  List(
    ScalarFunction("abs", { case Seq(NumberValue(t, n)) => NumberValue(abs(n.doubleValue)) }, NumberType),
    ScalarFunction("table", { case Seq(TableValue(d, _)) => ArrayValue(d map (r => ArrayValue(r.data))) }, ArrayType),
    ScalarFunction("typeof", { case Seq(v) => TextValue(v.vtyp.name) }, TextType),
    // Text functions
    ScalarFunction("lower", { case Seq(v) => TextValue(v.string.toLowerCase) }, TextType),
    ScalarFunction("upper", { case Seq(v) => TextValue(v.string.toUpperCase) }, TextType),
    ScalarFunction("length", { case Seq(v) => NumberValue(v.string.length) }, NumberType),
    ScalarFunction("array_length", { case Seq(ArrayValue(a)) => NumberValue(a.length) }, NumberType),
    ScalarFunction("trim", { case Seq(v) => TextValue(v.string.trim) }, TextType),
    ScalarFunction("ltrim", { case Seq(v) => TextValue(v.string.replaceAll("^\\s+", "")) }, TextType),
    ScalarFunction("rtrim", { case Seq(v) => TextValue(v.string.replaceAll("\\s+$", "")) }, TextType),
    ScalarFunction(
      "substring",
      {
        // substring(text, start)
        case Seq(TextValue(s), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0    = math.min(math.max(0, start - 1), s.length)
          TextValue(s.substring(i0))
        // substring(text, start, length)
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
      "left",
      { case Seq(TextValue(s), NumberValue(_, nNum)) =>
        val n  = math.max(0, nNum.intValue)
        val n2 = math.min(n, s.length)
        TextValue(s.substring(0, n2))
      },
      TextType,
    ),
    ScalarFunction(
      "right",
      { case Seq(TextValue(s), NumberValue(_, nNum)) =>
        val n  = math.max(0, nNum.intValue)
        val n2 = math.min(n, s.length)
        TextValue(s.substring(s.length - n2))
      },
      TextType,
    ),
    ScalarFunction(
      "replace",
      { case Seq(TextValue(s), TextValue(search), TextValue(repl)) => TextValue(s.replace(search, repl)) },
      TextType,
    ),
    ScalarFunction("concat", { case Seq(a, b) => TextValue(a.string + b.string) }, TextType),
    ScalarFunction(
      "repeat",
      { case Seq(TextValue(s), NumberValue(_, nNum)) => TextValue(s * math.max(0, nNum.intValue)) },
      TextType,
    ),
    ScalarFunction(
      "position",
      {
        case Seq(TextValue(substr), TextValue(s)) =>
          val idx = s.indexOf(substr)
          NumberValue(if idx < 0 then 0 else idx + 1) // 1-based; 0 when not found
      },
      NumberType,
    ),
    // Numeric functions
    ScalarFunction("ceil", { case Seq(NumberValue(_, n)) => NumberValue(math.ceil(n.doubleValue)) }, NumberType),
    ScalarFunction("ceiling", { case Seq(NumberValue(_, n)) => NumberValue(math.ceil(n.doubleValue)) }, NumberType),
    ScalarFunction("floor", { case Seq(NumberValue(_, n)) => NumberValue(math.floor(n.doubleValue)) }, NumberType),
    ScalarFunction(
      "round",
      {
        // round(x)
        case Seq(NumberValue(_, n)) => NumberValue(math.rint(n.doubleValue))
        // round(x, digits)
        case Seq(NumberValue(_, n), NumberValue(_, d)) =>
          val k   = d.intValue
          val m   = math.pow(10.0, k.toDouble)
          val res = if k >= 0 then math.rint(n.doubleValue * m) / m else math.rint(n.doubleValue / m) * m
          NumberValue(res)
      },
      NumberType,
    ),
    ScalarFunction(
      "trunc",
      {
        // trunc(x)
        case Seq(NumberValue(_, n)) =>
          NumberValue(if n.doubleValue >= 0 then math.floor(n.doubleValue) else math.ceil(n.doubleValue))
        // trunc(x, digits)
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
    ScalarFunction("sign", { case Seq(NumberValue(_, n)) => NumberValue(math.signum(n.doubleValue)) }, NumberType),
    ScalarFunction(
      "mod",
      { case Seq(NumberValue(_, a), NumberValue(_, b)) => NumberValue(a.doubleValue % b.doubleValue) },
      NumberType,
    ),
    ScalarFunction(
      "power",
      { case Seq(NumberValue(_, x), NumberValue(_, y)) => NumberValue(math.pow(x.doubleValue, y.doubleValue)) },
      NumberType,
    ),
    ScalarFunction("sqrt", { case Seq(NumberValue(_, n)) => NumberValue(math.sqrt(n.doubleValue)) }, NumberType),
    // Arrays
    ScalarFunction("array_length", { case Seq(ArrayValue(elems)) => NumberValue(elems.length) }, NumberType),
    ScalarFunction(
      "array_slice",
      {
        // array_slice(arr, start)
        case Seq(ArrayValue(elems), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0    = math.min(math.max(0, start - 1), elems.length)
          ArrayValue(elems.slice(i0, elems.length))
        // array_slice(arr, start, end)
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
      "array_append",
      { case Seq(ArrayValue(elems), v) => ArrayValue(elems :+ v) },
      ArrayType,
    ),
    ScalarFunction(
      "array_prepend",
      { case Seq(v, ArrayValue(elems)) => ArrayValue(v +: elems) },
      ArrayType,
    ),
    ScalarFunction(
      "array_concat",
      { case Seq(ArrayValue(a), ArrayValue(b)) => ArrayValue(a ++ b) },
      ArrayType,
    ),
    // Padding
    ScalarFunction(
      "lpad",
      {
        // lpad(text, len)
        case Seq(TextValue(s), NumberValue(_, lenNum)) =>
          val len     = math.max(0, lenNum.intValue)
          val needed  = math.max(0, len - s.length)
          val padStr  = " "
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val left    = (padStr * repeats).take(needed)
          TextValue((left + s).take(len))
        // lpad(text, len, pad)
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
      "rpad",
      {
        // rpad(text, len)
        case Seq(TextValue(s), NumberValue(_, lenNum)) =>
          val len     = math.max(0, lenNum.intValue)
          val needed  = math.max(0, len - s.length)
          val padStr  = " "
          val repeats = if needed == 0 then 0 else (needed + padStr.length - 1) / padStr.length
          val right   = (padStr * repeats).take(needed)
          TextValue((s + right).take(len))
        // rpad(text, len, pad)
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
      "substr",
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
    ScalarFunction("exp", { case Seq(NumberValue(_, n)) => NumberValue(math.exp(n.doubleValue)) }, NumberType),
    ScalarFunction("ln", { case Seq(NumberValue(_, n)) => NumberValue(math.log(n.doubleValue)) }, NumberType),
    ScalarFunction("log10", { case Seq(NumberValue(_, n)) => NumberValue(math.log10(n.doubleValue)) }, NumberType),
    ScalarFunction("random", { case Seq() => NumberValue(math.random) }, NumberType),
    // UUID functions
    ScalarFunction("gen_random_uuid", { case Seq() => UUIDValue.generate }, UUIDType),
    // Null-handling functions
    ScalarFunction(
      "coalesce",
      { case values =>
        values.find(!_.isNull).getOrElse(NullValue())
      },
      TextType, // Return type will be adjusted based on actual data
    ),
    ScalarFunction(
      "nullif",
      { case Seq(value1, value2) =>
        if value1.string == value2.string then NullValue() else value1
      },
      TextType, // Return type will be adjusted based on actual data
    ),
    // Additional string functions
    ScalarFunction(
      "split_part",
      { case Seq(TextValue(s), TextValue(delimiter), NumberValue(_, fieldNum)) =>
        val parts = s.split(java.util.regex.Pattern.quote(delimiter), -1)
        val field = fieldNum.intValue
        if field <= 0 || field > parts.length then TextValue("")
        else TextValue(parts(field - 1))
      },
      TextType,
    ),
    ScalarFunction(
      "reverse",
      { case Seq(v) => TextValue(v.string.reverse) },
      TextType,
    ),
    // Date/time functions
    ScalarFunction(
      "now",
      { case Seq() => TimestampValue(LocalDateTime.now(ZoneOffset.UTC)) },
      TimestampType,
    ),
    ScalarFunction(
      "current_date",
      { case Seq() => DateValue(LocalDate.now(ZoneOffset.UTC)) },
      DateType,
    ),
    ScalarFunction(
      "current_time",
      { case Seq() => TimeValue(LocalTime.now(ZoneOffset.UTC)) },
      TimeType,
    ),
    ScalarFunction(
      "date_part",
      {
        case Seq(TextValue(part), TimestampValue(ts)) =>
          part.toLowerCase match
            case "year"   => NumberValue(ts.getYear)
            case "month"  => NumberValue(ts.getMonthValue)
            case "day"    => NumberValue(ts.getDayOfMonth)
            case "hour"   => NumberValue(ts.getHour)
            case "minute" => NumberValue(ts.getMinute)
            case "second" => NumberValue(ts.getSecond)
            case _        => NumberValue(0)
        case Seq(TextValue(part), DateValue(d)) =>
          part.toLowerCase match
            case "year"  => NumberValue(d.getYear)
            case "month" => NumberValue(d.getMonthValue)
            case "day"   => NumberValue(d.getDayOfMonth)
            case _       => NumberValue(0)
        case Seq(TextValue(part), TimeValue(t)) =>
          part.toLowerCase match
            case "hour"   => NumberValue(t.getHour)
            case "minute" => NumberValue(t.getMinute)
            case "second" => NumberValue(t.getSecond)
            case _        => NumberValue(0)
      },
      NumberType,
    ),
    ScalarFunction(
      "make_date",
      { case Seq(NumberValue(_, y), NumberValue(_, m), NumberValue(_, d)) =>
        DateValue(LocalDate.of(y.intValue, m.intValue, d.intValue))
      },
      DateType,
    ),
    ScalarFunction(
      "make_time",
      { case Seq(NumberValue(_, h), NumberValue(_, m), NumberValue(_, s)) =>
        TimeValue(LocalTime.of(h.intValue, m.intValue, s.intValue))
      },
      TimeType,
    ),
    ScalarFunction(
      "octet_length",
      {
        case Seq(ByteaValue(data)) => NumberValue(data.length)
        case Seq(TextValue(s))     => NumberValue(s.getBytes("UTF-8").length)
      },
      NumberType,
    ),
    ScalarFunction(
      "encode",
      { case Seq(ByteaValue(data), TextValue(format)) =>
        format.toLowerCase match
          case "hex"    => TextValue(data.map(b => f"${b & 0xff}%02x").mkString)
          case "base64" => TextValue(java.util.Base64.getEncoder.encodeToString(data))
          case _        => sys.error(s"unsupported encoding format: $format")
      },
      TextType,
    ),
    ScalarFunction(
      "decode",
      { case Seq(TextValue(s), TextValue(format)) =>
        format.toLowerCase match
          case "hex" =>
            val bytes = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
            ByteaValue(bytes)
          case "base64" =>
            ByteaValue(java.util.Base64.getDecoder.decode(s))
          case _ => sys.error(s"unsupported encoding format: $format")
      },
      ByteaType,
    ),
    // greatest / least
    ScalarFunction(
      "greatest",
      { case values if values.nonEmpty =>
        values.filterNot(_.isNull) match
          case Seq() => NullValue()
          case vs    => vs.reduce((a, b) => if a.compare(b) >= 0 then a else b)
      },
      NumberType,
    ),
    ScalarFunction(
      "least",
      { case values if values.nonEmpty =>
        values.filterNot(_.isNull) match
          case Seq() => NullValue()
          case vs    => vs.reduce((a, b) => if a.compare(b) <= 0 then a else b)
      },
      NumberType,
    ),
    // concat_ws(separator, val1, val2, ...)
    ScalarFunction(
      "concat_ws",
      { case TextValue(sep) +: values =>
        val nonNull = values.filterNot(_.isNull).map(_.string)
        TextValue(nonNull.mkString(sep))
      },
      TextType,
    ),
    // initcap
    ScalarFunction(
      "initcap",
      { case Seq(v) =>
        val s = v.string
        val sb = new StringBuilder(s.length)
        var capitalizeNext = true
        for c <- s do
          if c.isWhitespace then
            sb += c
            capitalizeNext = true
          else if capitalizeNext then
            sb += c.toUpper
            capitalizeNext = false
          else
            sb += c.toLower
        TextValue(sb.toString)
      },
      TextType,
    ),
    // char_length / character_length (aliases for length)
    ScalarFunction("char_length", { case Seq(v) => NumberValue(v.string.length) }, NumberType),
    ScalarFunction("character_length", { case Seq(v) => NumberValue(v.string.length) }, NumberType),
    // ascii / chr
    ScalarFunction(
      "ascii",
      { case Seq(v) =>
        val s = v.string
        NumberValue(if s.isEmpty then 0 else s.charAt(0).toInt)
      },
      NumberType,
    ),
    ScalarFunction(
      "chr",
      { case Seq(NumberValue(_, n)) => TextValue(n.intValue.toChar.toString) },
      TextType,
    ),
    // starts_with / ends_with
    ScalarFunction(
      "starts_with",
      { case Seq(TextValue(s), TextValue(prefix)) => BooleanValue(s.startsWith(prefix)) },
      BooleanType,
    ),
    ScalarFunction(
      "ends_with",
      { case Seq(TextValue(s), TextValue(suffix)) => BooleanValue(s.endsWith(suffix)) },
      BooleanType,
    ),
    // regexp_replace(text, pattern, replacement)
    ScalarFunction(
      "regexp_replace",
      {
        // regexp_replace(text, pattern, replacement) - first match only
        case Seq(TextValue(s), TextValue(pattern), TextValue(replacement)) =>
          TextValue(s.replaceFirst(pattern, replacement))
        // regexp_replace(text, pattern, replacement, flags) - 'g' for global
        case Seq(TextValue(s), TextValue(pattern), TextValue(replacement), TextValue(flags)) =>
          if flags.contains("g") then TextValue(s.replaceAll(pattern, replacement))
          else TextValue(s.replaceFirst(pattern, replacement))
      },
      TextType,
    ),
    // regexp_match(text, pattern) - returns first match as array
    ScalarFunction(
      "regexp_match",
      { case Seq(TextValue(s), TextValue(pattern)) =>
        val m = java.util.regex.Pattern.compile(pattern).matcher(s)
        if m.find() then
          if m.groupCount() > 0 then
            ArrayValue((1 to m.groupCount()).map(i => Option(m.group(i)).map(TextValue(_)).getOrElse(NullValue()): Value).toIndexedSeq)
          else ArrayValue(IndexedSeq(TextValue(m.group(0))))
        else NullValue()
      },
      ArrayType,
    ),
    // date_trunc(field, timestamp)
    ScalarFunction(
      "date_trunc",
      {
        case Seq(TextValue(field), TimestampValue(ts)) =>
          TimestampValue(field.toLowerCase match
            case "year"    => LocalDateTime.of(ts.getYear, 1, 1, 0, 0, 0)
            case "quarter" =>
              val q = (ts.getMonthValue - 1) / 3 * 3 + 1
              LocalDateTime.of(ts.getYear, q, 1, 0, 0, 0)
            case "month"   => LocalDateTime.of(ts.getYear, ts.getMonthValue, 1, 0, 0, 0)
            case "week"    =>
              val d = ts.toLocalDate
              val dow = d.getDayOfWeek.getValue // Monday=1
              LocalDateTime.of(d.minusDays(dow - 1), LocalTime.MIDNIGHT)
            case "day"     => LocalDateTime.of(ts.toLocalDate, LocalTime.MIDNIGHT)
            case "hour"    => ts.truncatedTo(ChronoUnit.HOURS)
            case "minute"  => ts.truncatedTo(ChronoUnit.MINUTES)
            case "second"  => ts.truncatedTo(ChronoUnit.SECONDS)
            case _         => ts)
        case Seq(TextValue(field), DateValue(d)) =>
          DateValue(field.toLowerCase match
            case "year"    => LocalDate.of(d.getYear, 1, 1)
            case "quarter" =>
              val q = (d.getMonthValue - 1) / 3 * 3 + 1
              LocalDate.of(d.getYear, q, 1)
            case "month"   => LocalDate.of(d.getYear, d.getMonthValue, 1)
            case "week"    =>
              val dow = d.getDayOfWeek.getValue
              d.minusDays(dow - 1)
            case _         => d)
      },
      TimestampType,
    ),
    // age(timestamp, timestamp) -> interval
    ScalarFunction(
      "age",
      {
        case Seq(TimestampValue(t1), TimestampValue(t2)) =>
          IntervalValue(Duration.between(t2, t1))
        case Seq(DateValue(d1), DateValue(d2)) =>
          IntervalValue(Duration.ofDays(ChronoUnit.DAYS.between(d2, d1)))
        // age(timestamp) = age(now, timestamp)
        case Seq(TimestampValue(t)) =>
          IntervalValue(Duration.between(t, LocalDateTime.now(ZoneOffset.UTC)))
        case Seq(DateValue(d)) =>
          IntervalValue(Duration.ofDays(ChronoUnit.DAYS.between(d, LocalDate.now(ZoneOffset.UTC))))
      },
      IntervalType,
    ),
    // to_char(value, format)
    ScalarFunction(
      "to_char",
      {
        case Seq(TimestampValue(ts), TextValue(fmt)) =>
          TextValue(ts.format(sqlToJavaDateFormat(fmt)))
        case Seq(DateValue(d), TextValue(fmt)) =>
          TextValue(d.format(sqlToJavaDateFormat(fmt)))
        case Seq(TimeValue(t), TextValue(fmt)) =>
          TextValue(t.format(sqlToJavaDateFormat(fmt)))
        case Seq(NumberValue(_, n), TextValue(fmt)) =>
          TextValue(formatNumber(n, fmt))
      },
      TextType,
    ),
    // to_date(text, format)
    ScalarFunction(
      "to_date",
      { case Seq(TextValue(s), TextValue(fmt)) =>
        DateValue(LocalDate.parse(s, sqlToJavaDateFormat(fmt)))
      },
      DateType,
    ),
    // to_timestamp(text, format)
    ScalarFunction(
      "to_timestamp",
      { case Seq(TextValue(s), TextValue(fmt)) =>
        TimestampValue(LocalDateTime.parse(s, sqlToJavaDateFormat(fmt)))
      },
      TimestampType,
    ),
    // quote_literal / quote_ident
    ScalarFunction(
      "quote_literal",
      {
        case Seq(v) if v.isNull => NullValue()
        case Seq(v)             => TextValue("'" + v.string.replace("'", "''") + "'")
      },
      TextType,
    ),
    ScalarFunction(
      "quote_ident",
      { case Seq(v) => TextValue("\"" + v.string.replace("\"", "\"\"") + "\"") },
      TextType,
    ),
    // clock_timestamp
    ScalarFunction(
      "clock_timestamp",
      { case Seq() => TimestampValue(LocalDateTime.now(ZoneOffset.UTC)) },
      TimestampType,
    ),
    // regexp_split_to_array
    ScalarFunction(
      "regexp_split_to_array",
      { case Seq(TextValue(s), TextValue(pattern)) =>
        ArrayValue(s.split(pattern, -1).map(TextValue(_): Value).toIndexedSeq)
      },
      ArrayType,
    ),
    // math constants and functions
    ScalarFunction("pi", { case Seq() => NumberValue(math.Pi) }, NumberType),
    ScalarFunction(
      "log",
      {
        // log(value) = log base 10
        case Seq(NumberValue(_, n)) => NumberValue(math.log10(n.doubleValue))
        // log(base, value)
        case Seq(NumberValue(_, b), NumberValue(_, n)) =>
          NumberValue(math.log(n.doubleValue) / math.log(b.doubleValue))
      },
      NumberType,
    ),
    ScalarFunction("degrees", { case Seq(NumberValue(_, n)) => NumberValue(math.toDegrees(n.doubleValue)) }, NumberType),
    ScalarFunction("radians", { case Seq(NumberValue(_, n)) => NumberValue(math.toRadians(n.doubleValue)) }, NumberType),
    // trig functions
    ScalarFunction("sin", { case Seq(NumberValue(_, n)) => NumberValue(math.sin(n.doubleValue)) }, NumberType),
    ScalarFunction("cos", { case Seq(NumberValue(_, n)) => NumberValue(math.cos(n.doubleValue)) }, NumberType),
    ScalarFunction("tan", { case Seq(NumberValue(_, n)) => NumberValue(math.tan(n.doubleValue)) }, NumberType),
    ScalarFunction("asin", { case Seq(NumberValue(_, n)) => NumberValue(math.asin(n.doubleValue)) }, NumberType),
    ScalarFunction("acos", { case Seq(NumberValue(_, n)) => NumberValue(math.acos(n.doubleValue)) }, NumberType),
    ScalarFunction("atan", { case Seq(NumberValue(_, n)) => NumberValue(math.atan(n.doubleValue)) }, NumberType),
    ScalarFunction(
      "atan2",
      { case Seq(NumberValue(_, y), NumberValue(_, x)) => NumberValue(math.atan2(y.doubleValue, x.doubleValue)) },
      NumberType,
    ),
    // string_to_array / array_to_string
    ScalarFunction(
      "string_to_array",
      { case Seq(TextValue(s), TextValue(delimiter)) =>
        ArrayValue(s.split(java.util.regex.Pattern.quote(delimiter), -1).map(TextValue(_): Value).toIndexedSeq)
      },
      ArrayType,
    ),
    ScalarFunction(
      "array_to_string",
      { case Seq(ArrayValue(elems), TextValue(sep)) =>
        TextValue(elems.filterNot(_.isNull).map(_.string).mkString(sep))
      },
      TextType,
    ),
    // array_remove / array_position / array_contains
    ScalarFunction(
      "array_remove",
      { case Seq(ArrayValue(elems), v) => ArrayValue(elems.filterNot(_ == v)) },
      ArrayType,
    ),
    ScalarFunction(
      "array_position",
      { case Seq(ArrayValue(elems), v) =>
        val idx = elems.indexOf(v)
        if idx < 0 then NullValue() else NumberValue(idx + 1)
      },
      NumberType,
    ),
    ScalarFunction(
      "array_distinct",
      { case Seq(ArrayValue(elems)) => ArrayValue(elems.distinct) },
      ArrayType,
    ),
    // array_cat (alias for array_concat)
    ScalarFunction(
      "array_cat",
      { case Seq(ArrayValue(a), ArrayValue(b)) => ArrayValue(a ++ b) },
      ArrayType,
    ),
    // array_lower(a, dim) — always returns 1 (1-based indexing)
    ScalarFunction(
      "array_lower",
      { case Seq(ArrayValue(_), NumberValue(_, _)) => NumberValue(1) },
      NumberType,
    ),
    // array_upper(a, dim) — returns length
    ScalarFunction(
      "array_upper",
      { case Seq(ArrayValue(a), NumberValue(_, _)) => NumberValue(a.length) },
      NumberType,
    ),
    // array_ndims(a) — always 1 (flat arrays)
    ScalarFunction(
      "array_ndims",
      { case Seq(ArrayValue(_)) => NumberValue(1) },
      NumberType,
    ),
    // array_replace(a, old, new)
    ScalarFunction(
      "array_replace",
      { case Seq(ArrayValue(elems), oldVal, newVal) =>
        ArrayValue(elems.map(e => if e == oldVal then newVal else e))
      },
      ArrayType,
    ),
    // cardinality(a) — alias for array_length
    ScalarFunction(
      "cardinality",
      { case Seq(ArrayValue(elems)) => NumberValue(elems.length) },
      NumberType,
    ),
    // cbrt(x)
    ScalarFunction("cbrt", { case Seq(NumberValue(_, n)) => NumberValue(math.cbrt(n.doubleValue)) }, NumberType),
    // div(x, y) — integer division
    ScalarFunction(
      "div",
      { case Seq(NumberValue(_, x), NumberValue(_, y)) => NumberValue((x.longValue / y.longValue).toDouble) },
      NumberType,
    ),
    // factorial(n)
    ScalarFunction(
      "factorial",
      { case Seq(NumberValue(_, n)) =>
        val k = n.longValue
        var result = 1L
        for i <- 2L to k do result *= i
        NumberValue(result.toDouble)
      },
      NumberType,
    ),
    // gcd(a, b)
    ScalarFunction(
      "gcd",
      { case Seq(NumberValue(_, a), NumberValue(_, b)) =>
        @scala.annotation.tailrec
        def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
        NumberValue(math.abs(gcd(a.longValue, b.longValue)).toDouble)
      },
      NumberType,
    ),
    // lcm(a, b)
    ScalarFunction(
      "lcm",
      { case Seq(NumberValue(_, a), NumberValue(_, b)) =>
        @scala.annotation.tailrec
        def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
        val av = a.longValue
        val bv = b.longValue
        NumberValue((if av == 0 || bv == 0 then 0L else math.abs(av / gcd(av, bv) * bv)).toDouble)
      },
      NumberType,
    ),
    // hyperbolic trig functions
    ScalarFunction("sinh", { case Seq(NumberValue(_, n)) => NumberValue(math.sinh(n.doubleValue)) }, NumberType),
    ScalarFunction("cosh", { case Seq(NumberValue(_, n)) => NumberValue(math.cosh(n.doubleValue)) }, NumberType),
    ScalarFunction("tanh", { case Seq(NumberValue(_, n)) => NumberValue(math.tanh(n.doubleValue)) }, NumberType),
    // inverse hyperbolic trig functions
    ScalarFunction(
      "asinh",
      { case Seq(NumberValue(_, n)) =>
        val x = n.doubleValue
        NumberValue(math.log(x + math.sqrt(x * x + 1)))
      },
      NumberType,
    ),
    ScalarFunction(
      "acosh",
      { case Seq(NumberValue(_, n)) =>
        val x = n.doubleValue
        NumberValue(math.log(x + math.sqrt(x * x - 1)))
      },
      NumberType,
    ),
    ScalarFunction(
      "atanh",
      { case Seq(NumberValue(_, n)) =>
        val x = n.doubleValue
        NumberValue(0.5 * math.log((1 + x) / (1 - x)))
      },
      NumberType,
    ),
    // translate(s, from, to)
    ScalarFunction(
      "translate",
      { case Seq(TextValue(s), TextValue(from), TextValue(to)) =>
        val sb = new StringBuilder(s.length)
        for c <- s do
          val idx = from.indexOf(c)
          if idx < 0 then sb += c
          else if idx < to.length then sb += to.charAt(idx)
          // else: character is in `from` but beyond `to` — deleted
        TextValue(sb.toString)
      },
      TextType,
    ),
    // btrim(s, chars) — trim specific chars from both ends
    ScalarFunction(
      "btrim",
      {
        case Seq(TextValue(s)) => TextValue(s.trim)
        case Seq(TextValue(s), TextValue(chars)) =>
          val charSet = chars.toSet
          val start = s.indexWhere(c => !charSet.contains(c))
          if start < 0 then TextValue("")
          else
            val end = s.lastIndexWhere(c => !charSet.contains(c))
            TextValue(s.substring(start, end + 1))
      },
      TextType,
    ),
    // make_timestamp(y, mo, d, h, mi, s)
    ScalarFunction(
      "make_timestamp",
      { case Seq(NumberValue(_, y), NumberValue(_, mo), NumberValue(_, d), NumberValue(_, h), NumberValue(_, mi), NumberValue(_, s)) =>
        TimestampValue(LocalDateTime.of(y.intValue, mo.intValue, d.intValue, h.intValue, mi.intValue, s.intValue))
      },
      TimestampType,
    ),
    // make_interval(days, hours, mins, secs)
    ScalarFunction(
      "make_interval",
      {
        case Seq(NumberValue(_, days), NumberValue(_, hours), NumberValue(_, mins), NumberValue(_, secs)) =>
          IntervalValue(Duration.ofDays(days.longValue).plusHours(hours.longValue).plusMinutes(mins.longValue).plusSeconds(secs.longValue))
        case Seq(NumberValue(_, days), NumberValue(_, hours), NumberValue(_, mins)) =>
          IntervalValue(Duration.ofDays(days.longValue).plusHours(hours.longValue).plusMinutes(mins.longValue))
        case Seq(NumberValue(_, days), NumberValue(_, hours)) =>
          IntervalValue(Duration.ofDays(days.longValue).plusHours(hours.longValue))
        case Seq(NumberValue(_, days)) =>
          IntervalValue(Duration.ofDays(days.longValue))
      },
      IntervalType,
    ),
    // to_number(text, format) — parse numeric string
    ScalarFunction(
      "to_number",
      { case Seq(TextValue(s), TextValue(_)) =>
        val cleaned = s.replaceAll("[^0-9.eE+-]", "")
        NumberValue(cleaned.toDouble)
      },
      NumberType,
    ),
    // isfinite(date/timestamp) — always true (Java time has no infinities)
    ScalarFunction(
      "isfinite",
      {
        case Seq(_: DateValue)      => BooleanValue(true)
        case Seq(_: TimestampValue) => BooleanValue(true)
      },
      BooleanType,
    ),
    ScalarFunction(
      "overlay",
      {
        // overlay(string, replacement, start, count)
        case Seq(TextValue(s), TextValue(repl), NumberValue(_, startNum), NumberValue(_, countNum)) =>
          val start = math.max(1, startNum.intValue)
          val count = math.max(0, countNum.intValue)
          val i0 = math.min(start - 1, s.length)
          val i1 = math.min(i0 + count, s.length)
          TextValue(s.substring(0, i0) + repl + s.substring(i1))
        // overlay(string, replacement, start) — count defaults to length of replacement
        case Seq(TextValue(s), TextValue(repl), NumberValue(_, startNum)) =>
          val start = math.max(1, startNum.intValue)
          val i0 = math.min(start - 1, s.length)
          val i1 = math.min(i0 + repl.length, s.length)
          TextValue(s.substring(0, i0) + repl + s.substring(i1))
      },
      TextType,
    ),
    ScalarFunction(
      "width_bucket",
      { case Seq(NumberValue(_, v), NumberValue(_, lo), NumberValue(_, hi), NumberValue(_, cnt)) =>
        val value = v.doubleValue
        val low = lo.doubleValue
        val high = hi.doubleValue
        val count = cnt.intValue
        if count <= 0 then NumberValue(0)
        else if value < low then NumberValue(0)
        else if value >= high then NumberValue(count + 1)
        else NumberValue(((value - low) / (high - low) * count).toInt + 1)
      },
      NumberType,
    ),
    ScalarFunction(
      "get_byte",
      { case Seq(ByteaValue(data), NumberValue(_, offsetNum)) =>
        val offset = offsetNum.intValue
        if offset < 0 || offset >= data.length then sys.error(s"get_byte: index $offset out of range 0..${data.length - 1}")
        NumberValue(data(offset) & 0xff)
      },
      NumberType,
    ),
    ScalarFunction(
      "set_byte",
      { case Seq(ByteaValue(data), NumberValue(_, offsetNum), NumberValue(_, valNum)) =>
        val offset = offsetNum.intValue
        if offset < 0 || offset >= data.length then sys.error(s"set_byte: index $offset out of range 0..${data.length - 1}")
        val copy = data.clone()
        copy(offset) = valNum.intValue.toByte
        ByteaValue(copy)
      },
      ByteaType,
    ),
  ).map(f => f.name -> f).toMap ++ jsonScalarFunctions

// Convert SQL date format patterns to Java DateTimeFormatter patterns
private def sqlToJavaDateFormat(fmt: String): DateTimeFormatter =
  val javaFmt = fmt
    .replace("YYYY", "yyyy").replace("YY", "yy")
    .replace("MM", "MM").replace("DD", "dd")
    .replace("HH24", "HH").replace("HH12", "hh").replace("HH", "HH")
    .replace("MI", "mm").replace("SS", "ss")
    .replace("Month", "MMMM").replace("Mon", "MMM")
    .replace("Day", "EEEE").replace("Dy", "EEE")
    .replace("AM", "a").replace("PM", "a")
  DateTimeFormatter.ofPattern(javaFmt)

private def formatNumber(n: Number, fmt: String): String =
  if fmt.contains("FM") then
    val clean = fmt.replace("FM", "")
    formatNumberCore(n, clean).replaceAll("\\s+", "").replaceAll("^0+(?=\\d)", "")
  else formatNumberCore(n, fmt)

private def formatNumberCore(n: Number, fmt: String): String =
  val d = n.doubleValue
  if fmt.contains(".") then
    val decimalPlaces = fmt.length - fmt.indexOf('.') - 1
    val totalWidth    = fmt.length
    String.format(s"%${totalWidth}.${decimalPlaces}f", d)
  else
    val totalWidth = fmt.length
    String.format(s"%${totalWidth}.0f", d)
