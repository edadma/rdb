package io.github.edadma.petradb

import scala.concurrent.{Future, ExecutionContext}

trait Session:
  def execute(sql: String)(using ExecutionContext): Future[Seq[Result]]
  def close()(using ExecutionContext): Future[Unit]
