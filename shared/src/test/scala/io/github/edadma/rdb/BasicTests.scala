package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Testing {

  "auto int" in {
    test(
      """
        |CREATE TABLE t (
        | c1 INT AUTO PRIMARY KEY,
        | c2 INT
        |);
        |INSERT INTO t (c2) VALUES (123);
        |INSERT INTO t (c2) VALUES (456);
        |SELECT * FROM t;
        |""".trim.stripMargin,
    ) shouldBe
      """
        |List(
        |  CreateTableResult(table = "t"),
        |  InsertResult(
        |    obj = Map("c1" -> NumberValue(typ = IntType, value = 1)),
        |    table = TableValue(
        |      data = Vector(
        |        Row(
        |          data = Vector(NumberValue(typ = IntType, value = 1)),
        |          meta = Metadata(
        |            columns = Vector(
        |              ColumnMetadata(table = Some(value = "t"), name = "c1", typ = NumberType)
        |            )
        |          ),
        |          updater = None,
        |          deleter = None,
        |          mode = Return
        |        )
        |      ),
        |      meta = Metadata(
        |        columns = Vector(ColumnMetadata(table = Some(value = "t"), name = "c1", typ = NumberType))
        |      )
        |    )
        |  ),
        |  InsertResult(
        |    obj = Map("c1" -> NumberValue(typ = IntType, value = 2)),
        |    table = TableValue(
        |      data = Vector(
        |        Row(
        |          data = Vector(NumberValue(typ = IntType, value = 2)),
        |          meta = Metadata(
        |            columns = Vector(
        |              ColumnMetadata(table = Some(value = "t"), name = "c1", typ = NumberType)
        |            )
        |          ),
        |          updater = None,
        |          deleter = None,
        |          mode = Return
        |        )
        |      ),
        |      meta = Metadata(
        |        columns = Vector(ColumnMetadata(table = Some(value = "t"), name = "c1", typ = NumberType))
        |      )
        |    )
        |  ),
        |  QueryResult(
        |    table = TableValue(
        |      data = ArraySeq(
        |        Row(
        |          data = ArraySeq(
        |            NumberValue(typ = IntType, value = 1),
        |            NumberValue(typ = IntType, value = 123)
        |          ),
        |          meta = Metadata(
        |            columns = ArraySeq(
        |              ColumnMetadata(table = Some(value = "t"), name = "c1", typ = IntegerType),
        |              ColumnMetadata(table = Some(value = "t"), name = "c2", typ = IntegerType)
        |            )
        |          ),
        |          updater = Some(value = [MemoryDB Updater]),
        |          deleter = Some(value = [MemoryDB Deleter]),
        |          mode = Return
        |        ),
        |        Row(
        |          data = ArraySeq(
        |            NumberValue(typ = IntType, value = 2),
        |            NumberValue(typ = IntType, value = 456)
        |          ),
        |          meta = Metadata(
        |            columns = ArraySeq(
        |              ColumnMetadata(table = Some(value = "t"), name = "c1", typ = IntegerType),
        |              ColumnMetadata(table = Some(value = "t"), name = "c2", typ = IntegerType)
        |            )
        |          ),
        |          updater = Some(value = [MemoryDB Updater]),
        |          deleter = Some(value = [MemoryDB Deleter]),
        |          mode = Return
        |        )
        |      ),
        |      meta = Metadata(
        |        columns = ArraySeq(
        |          ColumnMetadata(table = Some(value = "t"), name = "c1", typ = IntegerType),
        |          ColumnMetadata(table = Some(value = "t"), name = "c2", typ = IntegerType)
        |        )
        |      )
        |    )
        |  )
        |)
        |""".trim.stripMargin
  }

}
