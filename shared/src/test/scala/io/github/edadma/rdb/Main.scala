package io.github.edadma.rdb

object Main extends App {
  val db = new MemoryDB("test")
  val t = db.create("t", RowMeta(Seq(ColumnMeta("a", IntType))))

  t.bulkInsert(Seq("a"),
               Seq(
                 Seq(IntValue(3))
               ))

  println(CollectStep(ScanStep(t)).value)
}
