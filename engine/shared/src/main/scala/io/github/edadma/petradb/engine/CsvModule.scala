package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.cross_platform.readFile
import io.github.edadma.csv.CSVRead

import scala.collection.mutable

/** Built-in virtual table module for CSV files.
  *
  * Usage:
  * {{{
  * CREATE VIRTUAL TABLE my_data USING csv('path/to/file.csv');
  * CREATE VIRTUAL TABLE my_data USING csv('path/to/file.csv', 'no_header');
  * CREATE VIRTUAL TABLE my_data USING csv('path/to/file.csv', 'header', '|');
  * }}}
  */
object CsvModule extends VirtualTableModule:
  def create(tableName: String, args: Seq[String]): VirtualTableProvider =
    if args.isEmpty then sys.error("csv module requires at least 1 argument: file path")
    val filePath = args.head
    val hasHeader = args.lift(1).map(_.toLowerCase) match
      case Some("no_header") => false
      case _                 => true
    val delimiter = args.lift(2).flatMap(_.headOption).getOrElse(',')

    val content = readFile(filePath)

    // Parse header to determine columns
    val firstRows = mutable.ArrayBuffer[Seq[String]]()
    CSVRead.fromStringStreamed(content, { row => firstRows += row }, delimiter)

    val columnNames =
      if firstRows.isEmpty then Vector.empty[String]
      else if hasHeader then
        firstRows.head.zipWithIndex.map { case (name, i) =>
          val trimmed = name.trim
          if trimmed.nonEmpty then trimmed else s"column${i + 1}"
        }.toVector
      else
        (1 to firstRows.head.length).map(i => s"column$i").toVector

    val columns = columnNames.map(n => (n, TextType))

    VirtualTableProvider(
      columns,
      () => {
        val rows = mutable.ArrayBuffer[Seq[String]]()
        val currentContent = readFile(filePath)
        CSVRead.fromStringStreamed(currentContent, { row => rows += row }, delimiter)
        rows.iterator.drop(if hasHeader then 1 else 0).map { row =>
          columnNames.indices.map { i =>
            if i < row.length then
              val s = row(i)
              if s.isEmpty then NullValue() else TextValue(s)
            else NullValue()
          }.toIndexedSeq
        }
      },
    )
