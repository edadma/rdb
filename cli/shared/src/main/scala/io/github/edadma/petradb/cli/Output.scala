package io.github.edadma.petradb.cli

import io.github.edadma.petradb.*
import io.github.edadma.table.TextTable

object Output:
  def printResult(result: Result): Unit =
    result match
      case QueryResult(table)       => println(tableString(table))
      case InsertResult(_, table)   =>
        if table.meta.width > 0 then println(tableString(table))
        else println("INSERT 0 1")
      case CreateTableResult(name)  => println(s"CREATE TABLE")
      case CreateIndexResult(name)  => println(s"CREATE INDEX")
      case DropTableResult(name)    => println(s"DROP TABLE")
      case DropIndexResult(name)    => println(s"DROP INDEX")
      case DropTypeResult(name)     => println(s"DROP TYPE")
      case CreateTypeResult(name)   => println(s"CREATE TYPE")
      case UpdateResult(n)          => println(s"UPDATE $n")
      case DeleteResult(n)          => println(s"DELETE $n")
      case AlterTableResult()       => println(s"ALTER TABLE")
      case BeginResult              => println(s"BEGIN")
      case CommitResult             => println(s"COMMIT")
      case RollbackResult           => println(s"ROLLBACK")
      case CopyResult(n)            => println(s"COPY $n")
      case CreateViewResult(name)   => println(s"CREATE VIEW")
      case DropViewResult(name)     => println(s"DROP VIEW")

  def listTables(db: DB): Unit =
    val names = db.tableNames.toSeq.sorted
    if names.isEmpty then println("No tables.")
    else
      val t = new TextTable:
        header("Table")
      for name <- names do t.row(name)
      println(t.toString)

  def listViews(db: DB): Unit =
    val names = db.viewNames.toSeq.sorted
    if names.isEmpty then println("No views.")
    else
      val t = new TextTable:
        header("View")
      for name <- names do t.row(name)
      println(t.toString)

  def describeTable(db: DB, name: String): Unit =
    db.getTable(name) match
      case None =>
        db.getView(name) match
          case None => println(s"Table or view '$name' not found.")
          case Some(sql) =>
            println(s"View \"$name\"")
            println(s"  AS $sql")
      case Some(table) =>
        val t = new TextTable:
          headerSeq(Seq("Column", "Type", "Nullable"))
        for col <- table.columns do
          t.rowSeq(Seq(col.name, col.typ.name, if col.required then "not null" else "null"))
        println(s"Table \"$name\"")
        println(t.toString)
