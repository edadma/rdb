package io.github.edadma.rdb

import scala.annotation.tailrec

val jsonScalarFunctions: Map[String, ScalarFunction] =
  List(
    ScalarFunction(
      "jsonb_typeof",
      { case Seq(v) =>
        TextValue(v match
          case _: ObjectValue  => "object"
          case _: ArrayValue   => "array"
          case _: TextValue    => "string"
          case _: NumberValue  => "number"
          case _: BooleanValue => "boolean"
          case _: NullValue    => "null"
          case _               => "unknown")
      },
      TextType,
    ),
    ScalarFunction(
      "json_typeof",
      { case Seq(v) =>
        TextValue(v match
          case _: ObjectValue  => "object"
          case _: ArrayValue   => "array"
          case _: TextValue    => "string"
          case _: NumberValue  => "number"
          case _: BooleanValue => "boolean"
          case _: NullValue    => "null"
          case _               => "unknown")
      },
      TextType,
    ),
    ScalarFunction(
      "jsonb_array_length",
      { case Seq(ArrayValue(data)) => NumberValue(data.length) },
      NumberType,
    ),
    ScalarFunction(
      "jsonb_object_keys",
      { case Seq(ObjectValue(props)) => ArrayValue(props.map { case (k, _) => TextValue(k): Value }.toIndexedSeq) },
      ArrayType,
    ),
    ScalarFunction(
      "jsonb_keys",
      { case Seq(ObjectValue(props)) => ArrayValue(props.map { case (k, _) => TextValue(k): Value }.toIndexedSeq) },
      ArrayType,
    ),
    ScalarFunction(
      "jsonb_extract_path",
      { case Seq(json, rest*) =>
        val keys = rest.map(_.string)
        jsonNavigatePath(json, keys)
      },
      ObjectType,
    ),
    ScalarFunction(
      "jsonb_extract_path_text",
      { case Seq(json, rest*) =>
        val keys = rest.map(_.string)
        jsonNavigatePath(json, keys) match
          case NullValue() => NullValue()
          case v           => TextValue(v.string)
      },
      TextType,
    ),
    ScalarFunction(
      "jsonb_set",
      {
        case Seq(target, ArrayValue(pathElems), newVal, BooleanValue(createMissing)) =>
          jsonSet(target, pathElems.map(_.string), newVal, createMissing)
        case Seq(target, ArrayValue(pathElems), newVal) =>
          jsonSet(target, pathElems.map(_.string), newVal, createMissing = true)
      },
      ObjectType,
    ),
    ScalarFunction(
      "jsonb_insert",
      {
        case Seq(target, ArrayValue(pathElems), newVal, BooleanValue(insertAfter)) =>
          jsonInsert(target, pathElems.map(_.string), newVal, insertAfter)
        case Seq(target, ArrayValue(pathElems), newVal) =>
          jsonInsert(target, pathElems.map(_.string), newVal, insertAfter = false)
      },
      ObjectType,
    ),
    ScalarFunction(
      "jsonb_strip_nulls",
      { case Seq(v) => stripNulls(v) },
      ObjectType,
    ),
    ScalarFunction(
      "jsonb_pretty",
      { case Seq(v) => TextValue(prettyJson(v, 0)) },
      TextType,
    ),
    ScalarFunction(
      "jsonb_build_object",
      { case pairs =>
        if pairs.length % 2 != 0 then sys.error("jsonb_build_object requires even number of arguments")
        val props = pairs.grouped(2).map { case Seq(k, v) => (k.string, v) }.toSeq
        ObjectValue(props)
      },
      ObjectType,
    ),
    ScalarFunction(
      "jsonb_build_array",
      { case elems => ArrayValue(elems.toIndexedSeq) },
      ArrayType,
    ),
    ScalarFunction(
      "to_jsonb",
      { case Seq(v) => toJsonValue(v) },
      ObjectType,
    ),
    ScalarFunction(
      "to_json",
      { case Seq(v) => toJsonValue(v) },
      ObjectType,
    ),
  ).map(f => f.name -> f).toMap

private def jsonNavigatePath(v: Value, keys: Seq[String]): Value =
  @tailrec
  def go(cur: Value, remaining: Seq[String]): Value =
    if remaining.isEmpty then cur
    else
      cur match
        case ObjectValue(props) =>
          props.collectFirst { case (k, vv) if k == remaining.head => vv } match
            case Some(next) => go(next, remaining.tail)
            case None       => NullValue()
        case ArrayValue(data) =>
          scala.util.Try(remaining.head.toInt).toOption match
            case Some(idx) =>
              val resolved = if idx < 0 then data.length + idx else idx
              if resolved >= 0 && resolved < data.length then go(data(resolved), remaining.tail)
              else NullValue()
            case None => NullValue()
        case _ => NullValue()
  go(v, keys)

private def stripNulls(v: Value): Value =
  v match
    case ObjectValue(props) =>
      ObjectValue(props.filter(!_._2.isNull).map { case (k, vv) => (k, stripNulls(vv)) })
    case ArrayValue(data) =>
      ArrayValue(data.map(stripNulls))
    case other => other

private def prettyJson(v: Value, indent: Int): String =
  val pad = "    " * indent
  val innerPad = "    " * (indent + 1)
  v match
    case ObjectValue(props) =>
      if props.isEmpty then "{}"
      else
        val entries = props.map { case (k, vv) =>
          s"""$innerPad"$k": ${prettyJson(vv, indent + 1)}"""
        }
        s"{\n${entries.mkString(",\n")}\n$pad}"
    case ArrayValue(data) =>
      if data.isEmpty then "[]"
      else
        val entries = data.map(e => s"$innerPad${prettyJson(e, indent + 1)}")
        s"[\n${entries.mkString(",\n")}\n$pad]"
    case TextValue(s) => s"\"$s\""
    case NullValue()  => "null"
    case v            => v.string

private def jsonSet(target: Value, path: Seq[String], newVal: Value, createMissing: Boolean): Value =
  if path.isEmpty then newVal
  else
    target match
      case ObjectValue(props) =>
        val key = path.head
        val existing = props.collectFirst { case (k, v) if k == key => v }
        existing match
          case Some(child) if path.length > 1 =>
            ObjectValue(props.map { case (k, v) =>
              if k == key then (k, jsonSet(v, path.tail, newVal, createMissing)) else (k, v)
            })
          case Some(_) =>
            ObjectValue(props.map { case (k, v) =>
              if k == key then (k, newVal) else (k, v)
            })
          case None if createMissing =>
            if path.length > 1 then ObjectValue(props :+ (key, jsonSet(ObjectValue(Seq.empty), path.tail, newVal, createMissing)))
            else ObjectValue(props :+ (key, newVal))
          case None => target
      case ArrayValue(data) =>
        scala.util.Try(path.head.toInt).toOption match
          case Some(idx) =>
            val resolved = if idx < 0 then data.length + idx else idx
            if resolved >= 0 && resolved < data.length then
              if path.length > 1 then ArrayValue(data.updated(resolved, jsonSet(data(resolved), path.tail, newVal, createMissing)))
              else ArrayValue(data.updated(resolved, newVal))
            else target
          case None => target
      case _ => target

private def jsonInsert(target: Value, path: Seq[String], newVal: Value, insertAfter: Boolean): Value =
  if path.isEmpty then target
  else if path.length == 1 then
    target match
      case ArrayValue(data) =>
        scala.util.Try(path.head.toInt).toOption match
          case Some(idx) =>
            val resolved = if idx < 0 then data.length + idx else idx
            val pos = if insertAfter then resolved + 1 else resolved
            val clampedPos = math.max(0, math.min(pos, data.length))
            ArrayValue((data.take(clampedPos) :+ newVal) ++ data.drop(clampedPos))
          case None => target
      case ObjectValue(props) =>
        val key = path.head
        if props.exists(_._1 == key) then target
        else ObjectValue(props :+ (key, newVal))
      case _ => target
  else
    target match
      case ObjectValue(props) =>
        val key = path.head
        ObjectValue(props.map { case (k, v) =>
          if k == key then (k, jsonInsert(v, path.tail, newVal, insertAfter)) else (k, v)
        })
      case ArrayValue(data) =>
        scala.util.Try(path.head.toInt).toOption match
          case Some(idx) =>
            val resolved = if idx < 0 then data.length + idx else idx
            if resolved >= 0 && resolved < data.length then
              ArrayValue(data.updated(resolved, jsonInsert(data(resolved), path.tail, newVal, insertAfter)))
            else target
          case None => target
      case _ => target

private def toJsonValue(v: Value): Value =
  v match
    case _: ObjectValue | _: ArrayValue | _: NullValue | _: BooleanValue | _: NumberValue => v
    case TextValue(s) => TextValue(s)
    case other        => TextValue(other.string)
