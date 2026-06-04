package io.github.edadma.petradb

import scala.concurrent.{Future, ExecutionContext}

trait Session:
  def execute(sql: String)(using ExecutionContext): Future[Seq[Result]]

  /** Execute SQL with positional bind parameters. Use `$1`, `$2`, … in the SQL; each placeholder
    * resolves to the correspondingly-indexed `Value`. Binding applies to DML and queries
    * (SELECT/INSERT/UPDATE/DELETE, including RETURNING and ON CONFLICT); parameter positions inside
    * DDL, CALL, and COPY are not substituted.
    */
  def execute(sql: String, params: Seq[Value])(using ExecutionContext): Future[Seq[Result]]

  def close()(using ExecutionContext): Future[Unit]
