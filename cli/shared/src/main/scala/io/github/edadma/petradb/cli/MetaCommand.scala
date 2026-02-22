package io.github.edadma.petradb.cli

enum MetaCommand:
  case ListTables
  case DescribeTable(name: String)
  case Quit
  case Include(path: String)
  case DumpSchema
  case Unknown(cmd: String)

object MetaCommand:
  def parse(input: String): MetaCommand =
    val trimmed = input.trim
    if trimmed == "\\dt" then ListTables
    else if trimmed == "\\q" then Quit
    else if trimmed == "\\dump" then DumpSchema
    else if trimmed.startsWith("\\d ") then
      val name = trimmed.drop(3).trim
      if name.nonEmpty then DescribeTable(name) else Unknown(trimmed)
    else if trimmed.startsWith("\\i ") then
      val path = trimmed.drop(3).trim
      if path.nonEmpty then Include(path) else Unknown(trimmed)
    else Unknown(trimmed)
