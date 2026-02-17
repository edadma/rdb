package io.github.edadma.rdb

import scala.math.*

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, ZoneOffset}

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
  ).map(f => f.name -> f).toMap
