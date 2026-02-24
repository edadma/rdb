package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.{Result, ColumnMetadata, ColumnSpec, PrimaryKeySpec}
import io.github.edadma.petradb.client
import io.github.edadma.petradb.client.{Session => ClientSession, SessionOptions}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

class PetraServerConnection(
  host: String,
  port: Int,
  val username: String,
  password: String,
) extends AbstractConnection:

  private val timeout                       = 30.seconds
  private given ExecutionContext            = ExecutionContext.global
  val url: String                           = s"jdbc:petradb://$host:$port"

  private val clientSession = new ClientSession(
    SessionOptions(
      host     = host,
      port     = port,
      username = if username.nonEmpty then Some(username) else None,
      password = if password.nonEmpty then Some(password) else None,
    ),
  )

  Await.result(clientSession.connect(), timeout)

  def execute(sql: String): Seq[Result] =
    Await.result(clientSession.execute(sql), timeout)

  def doClose(): Unit =
    Await.result(clientSession.close(), timeout)

  def tableNames: Iterable[String]                         = Iterable.empty
  def tableColumns(tableName: String): Seq[ColumnMetadata] = Seq.empty
  def tableColumnSpecs(tableName: String): Seq[ColumnSpec]      = Seq.empty
  def tablePrimaryKey(tableName: String): Option[PrimaryKeySpec] = None
