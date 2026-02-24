package io.github.edadma.petradb.jdbc

import java.util.Properties
import java.util.logging.Logger

class PetraDriver extends java.sql.Driver:

  def acceptsURL(url: String): Boolean =
    url != null && (url.startsWith("jdbc:petradb://") || url.startsWith("jdbc:petradb:file:"))

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
        else (hostPort, 5432)
      val username = Option(props.getProperty("user")).getOrElse("")
      val password = Option(props.getProperty("password")).getOrElse("")
      new PetraServerConnection(host, port, username, password)
    else
      val path = url.stripPrefix("jdbc:petradb:file:")
      new PetraFileConnection(path)

  def getPropertyInfo(url: String, info: Properties): Array[java.sql.DriverPropertyInfo] = Array.empty

  def getMajorVersion(): Int = 1
  def getMinorVersion(): Int = 0
  def jdbcCompliant(): Boolean = false
  def getParentLogger(): Logger = throw java.sql.SQLFeatureNotSupportedException()

object PetraDriver:
  java.sql.DriverManager.registerDriver(new PetraDriver())
