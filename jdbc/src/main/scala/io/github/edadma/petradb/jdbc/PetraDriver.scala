package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.DefaultPort
import java.util.Properties
import java.util.logging.Logger

class PetraDriver extends java.sql.Driver:
  PetraDriver.ensureRegistered(this)

  def acceptsURL(url: String): Boolean =
    url != null && (url.startsWith("jdbc:petradb://") || url.startsWith("jdbc:petradb:file:") || url == "jdbc:petradb:memory")

  def connect(url: String, info: Properties): java.sql.Connection =
    if !acceptsURL(url) then return null
    val props = if info != null then info else new Properties()

    if url.startsWith("jdbc:petradb://") then
      val rest  = url.stripPrefix("jdbc:petradb://")
      val slash = rest.indexOf('/')
      val hostPort = if slash >= 0 then rest.substring(0, slash) else rest
      val colon = hostPort.lastIndexOf(':')
      val (host, port) =
        if colon >= 0 then (hostPort.substring(0, colon), hostPort.substring(colon + 1).toInt)
        else (hostPort, DefaultPort)
      val username = Option(props.getProperty("user")).getOrElse("")
      val password = Option(props.getProperty("password")).getOrElse("")
      new PetraServerConnection(host, port, username, password)
    else if url == "jdbc:petradb:memory" then
      val key = s"memory:${java.util.UUID.randomUUID()}"
      new PetraFileConnection(key, ":memory:")
    else
      val path = url.stripPrefix("jdbc:petradb:file:")
      val key  = new java.io.File(path).getCanonicalPath
      new PetraFileConnection(key, path)

  def getPropertyInfo(url: String, info: Properties): Array[java.sql.DriverPropertyInfo] = Array.empty

  def getMajorVersion(): Int = 1
  def getMinorVersion(): Int = 0
  def jdbcCompliant(): Boolean = false
  def getParentLogger(): Logger = throw java.sql.SQLFeatureNotSupportedException()

object PetraDriver:
  @volatile private var registered = false
  def ensureRegistered(instance: PetraDriver): Unit =
    if !registered then
      registered = true
      java.sql.DriverManager.registerDriver(instance)
