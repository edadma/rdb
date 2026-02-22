package io.github.edadma.petradb.server

import io.github.edadma.petradb.{MemoryDB, Session, Platform}

import scala.collection.mutable

class SessionManager:
  private val sessions = mutable.Map[String, Session]()
  private val db = new MemoryDB

  def getSession(id: String): Session =
    sessions.getOrElseUpdate(id, db.connect())

  def createSession(): (String, Session) =
    val id = Platform.randomUUID
    val session = db.connect()
    sessions(id) = session
    (id, session)

  def closeSession(id: String): Boolean =
    sessions.remove(id).isDefined

  def transientSession(): Session = db.connect()

  def closeAll(): Unit =
    sessions.clear()
