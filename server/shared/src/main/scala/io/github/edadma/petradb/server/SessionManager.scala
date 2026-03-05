package io.github.edadma.petradb.server

import io.github.edadma.petradb.Platform
import io.github.edadma.petradb.engine.{DB, Session}

import scala.collection.mutable

class SessionManager(val db: DB, maxSessions: Int = 0):
  private val sessions = mutable.Map[String, Session]()

  def isFull: Boolean = maxSessions > 0 && sessions.size >= maxSessions

  def getSession(id: String): Option[Session] =
    sessions.get(id) match
      case some @ Some(_) => some
      case None =>
        if isFull then None
        else
          val s = db.connect()
          sessions(id) = s
          Some(s)

  def createSession(): Option[(String, Session)] =
    if isFull then None
    else
      val id = Platform.randomUUID
      val session = db.connect()
      sessions(id) = session
      Some((id, session))

  def closeSession(id: String): Boolean =
    sessions.remove(id).isDefined

  def transientSession(): Session = db.connect()

  def closeAll(): Unit =
    sessions.clear()
