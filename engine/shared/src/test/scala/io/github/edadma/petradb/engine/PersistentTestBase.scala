package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import io.github.edadma.cross_platform.{createTempFile, deleteFile}
import scala.compiletime.uninitialized

trait PersistentTestBase extends AnyFreeSpec with Matchers with BeforeAndAfterEach:
  protected var tmpFile: String = uninitialized
  protected val pageSize = 4096

  override def beforeEach(): Unit =
    tmpFile = createTempFile("petradb_test_", ".db")
    deleteFile(tmpFile) // FilePageStore.create needs a non-existent path

  override def afterEach(): Unit =
    try deleteFile(tmpFile)
    catch case _: Exception => ()
