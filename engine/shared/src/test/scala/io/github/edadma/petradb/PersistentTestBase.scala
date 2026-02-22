package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import io.github.edadma.cross_platform.{createTempFile, deleteFile}
import java.io.{ByteArrayOutputStream, PrintStream}

import scala.compiletime.uninitialized

trait PersistentTestBase extends AnyFreeSpec with Matchers with BeforeAndAfterEach:
  protected def suppressStderr[A](block: => A): A =
    Console.withErr(new PrintStream(new ByteArrayOutputStream()))(block)


  protected var tmpFile: String = uninitialized
  protected val pageSize = 4096

  override def beforeEach(): Unit =
    tmpFile = createTempFile("petradb_test_", ".db")
    deleteFile(tmpFile) // FilePageStore.create needs a non-existent path

  override def afterEach(): Unit =
    try deleteFile(tmpFile)
    catch case _: Exception => ()
