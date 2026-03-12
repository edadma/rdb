package io.github.edadma.petradb.cli

import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.tableString

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
      case CreateViewResult(name)     => println(s"CREATE VIEW")
      case DropViewResult(name)       => println(s"DROP VIEW")
      case CreateSequenceResult(name) => println(s"CREATE SEQUENCE")
      case DropSequenceResult(name)   => println(s"DROP SEQUENCE")
