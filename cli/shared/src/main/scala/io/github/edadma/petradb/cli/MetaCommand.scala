package io.github.edadma.petradb.cli

enum MetaCommand:
  case ListTables
  case ListViews
  case ListSequences
  case ListIndexes
  case DescribeTable(name: String)
  case Quit
  case Include(path: String)
  case DumpSchema
  case Copy(args: String)
  case Timing
  case Unknown(cmd: String)

object MetaCommand:
  def parse(input: String): MetaCommand =
    val trimmed = input.trim
    if trimmed == "\\dt" then ListTables
    else if trimmed == "\\dv" then ListViews
    else if trimmed == "\\ds" then ListSequences
    else if trimmed == "\\di" then ListIndexes
    else if trimmed == "\\q" then Quit
    else if trimmed == "\\dump" then DumpSchema
    else if trimmed.startsWith("\\d ") then
      val name = trimmed.drop(3).trim
      if name.nonEmpty then DescribeTable(name) else Unknown(trimmed)
    else if trimmed.startsWith("\\i ") then
      val path = trimmed.drop(3).trim
      if path.nonEmpty then Include(path) else Unknown(trimmed)
    else if trimmed.startsWith("\\copy ") then
      val args = trimmed.drop(6).trim
      if args.nonEmpty then Copy(args) else Unknown(trimmed)
    else if trimmed == "\\timing" then Timing
    else Unknown(trimmed)
