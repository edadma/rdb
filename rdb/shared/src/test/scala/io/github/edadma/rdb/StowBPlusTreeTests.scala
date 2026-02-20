package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import io.github.edadma.stow.{FilePageStore, NoPage}
import io.github.edadma.cross_platform.{createTempFile, deleteFile}

import scala.compiletime.uninitialized

class StowBPlusTreeTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach:

  private var tmpFile: String = uninitialized
  private val pageSize = 4096

  override def beforeEach(): Unit =
    tmpFile = createTempFile("stow_bpt_", ".db")
    deleteFile(tmpFile)

  override def afterEach(): Unit =
    try deleteFile(tmpFile)
    catch case _: Exception => ()

  "basic insert and search" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    var treeRecordPage = 0
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](5, store, batch, StowCodec.int, StowCodec.int)
      treeRecordPage = tree.treeRecordPage

      tree.insert(10, 100)
      tree.insert(20, 200)
      tree.insert(5, 50)

      tree.search(10) shouldBe Some(100)
      tree.search(20) shouldBe Some(200)
      tree.search(5) shouldBe Some(50)
      tree.search(99) shouldBe None
    }
    store.close()
  }

  "wellConstructed after inserts" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)

      for i <- 1 to 20 do
        tree.insert(i, i * 10)

      tree.wellConstructed shouldBe "true"
    }
    store.close()
  }

  "insert and delete" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)

      for i <- 1 to 10 do tree.insert(i, i * 10)

      tree.delete(5) shouldBe true
      tree.search(5) shouldBe None
      tree.delete(5) shouldBe false
      tree.search(3) shouldBe Some(30)
      tree.wellConstructed shouldBe "true"
    }
    store.close()
  }

  "iterator" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)

      val entries = Seq(5, 3, 8, 1, 4, 7, 2, 6, 9, 10)
      for e <- entries do tree.insert(e, e * 10)

      val result = tree.iterator.toList
      result shouldBe (1 to 10).map(i => (i, i * 10)).toList
    }
    store.close()
  }

  "bounded iterator" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)
      import io.github.edadma.bptree.Bound

      for i <- 1 to 20 do tree.insert(i, i * 10)

      val range = tree.boundedIterator((Bound.Gte, 5), (Bound.Lt, 10)).toList
      range shouldBe (5 to 9).map(i => (i, i * 10)).toList
    }
    store.close()
  }

  "insertIfNotFound" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)

      tree.insertIfNotFound(10, 100) shouldBe false
      tree.insertIfNotFound(10, 200) shouldBe true
      tree.search(10) shouldBe Some(100) // original value retained
    }
    store.close()
  }

  "reopen store preserves tree" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    var treePage = 0
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](5, store, batch, StowCodec.int, StowCodec.int)
      treePage = tree.treeRecordPage

      for i <- 1 to 15 do tree.insert(i, i * 100)
      tree.wellConstructed shouldBe "true"

      // Store treePage in metaRoot so we can find it after reopen
      batch.setMetaRoot(treePage)
    }
    store.close()

    // Reopen
    val store2 = FilePageStore.open(tmpFile)
    val treePage2 = store2.metaRoot
    treePage2 shouldBe treePage

    store2.modify { batch =>
      val tree = StowBPlusTree.open[Int, Int](5, store2, batch, treePage2, StowCodec.int, StowCodec.int)
      tree.wellConstructed shouldBe "true"

      for i <- 1 to 15 do
        tree.search(i) shouldBe Some(i * 100)

      // Insert more
      tree.insert(16, 1600)
      tree.search(16) shouldBe Some(1600)
      tree.wellConstructed shouldBe "true"
    }
    store2.close()
  }

  "stress test" in {
    val store = FilePageStore.create(tmpFile, pageSize)
    store.modify { batch =>
      val tree = StowBPlusTree.create[Int, Int](4, store, batch, StowCodec.int, StowCodec.int)

      val keys = scala.util.Random(42).shuffle((1 to 100).toList)
      for k <- keys do
        tree.insert(k, k * 10)
        tree.wellConstructed shouldBe "true"

      tree.iterator.map(_._1).toList shouldBe (1 to 100).toList

      // Delete half
      val toDelete = scala.util.Random(43).shuffle((1 to 100).toList).take(50)
      for k <- toDelete do
        tree.delete(k) shouldBe true
        tree.wellConstructed shouldBe "true"

      val remaining = (1 to 100).filterNot(toDelete.toSet).sorted
      tree.iterator.map(_._1).toList shouldBe remaining
    }
    store.close()
  }
