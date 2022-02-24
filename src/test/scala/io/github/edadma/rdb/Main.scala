package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

object Main extends App:

  val db = MemoryDB("test")
//  val t = db.create("t", Seq(ColumnSpec("t_id", NumberType), ColumnSpec("t_text", StringType)))
//
//  t.bulkInsert(
//    Seq("t_id", "t_text"),
//    Seq(
//      Seq(NumberValue(1), StringValue("t1")),
//      Seq(NumberValue(2), StringValue("t2"))
//    )
//  )

//  val u = db.create("u", Seq(ColumnSpec("u_id", NumberType), ColumnSpec("u_text", StringType)))
//
//  u.bulkInsert(
//    Seq("u_id", "u_text"),
//    Seq(
//      Seq(NumberValue(1), StringValue("u1")),
//      Seq(NumberValue(2), StringValue("u2")),
//      Seq(NumberValue(3), StringValue("u3"))
//    )
//  )

  val e =
    db.create("e", Seq(ColumnSpec("e_id", NumberType), ColumnSpec("name", StringType), ColumnSpec("m_id", NumberType)))

  e.bulkInsert(
    Seq("e_id", "name", "m_id"),
    Seq(
      Seq(NumberValue(1), StringValue("emp1"), NumberValue(2)),
      Seq(NumberValue(2), StringValue("mgr1"), NumberValue(3)),
      Seq(NumberValue(3), StringValue("mgr2"), NullValue),
      Seq(NumberValue(4), StringValue("emp2"), NumberValue(3))
    )
  )

  val sql =
//    SelectExpr(
//      ArraySeq(ApplyExpr(Ident("sum"), Seq(ColumnExpr(Ident("e_id"))))),
//      Seq(
//        TableOperator(Ident("e"))
//      ),
//      None,
//      None,
//      None
//    )

    SQLSelectExpr(
      ArraySeq(ColumnExpr(Ident("e_id")), ApplyExpr(Ident("sum"), Seq(ColumnExpr(Ident("e_id"))))),
      Seq(
        TableOperator(Ident("e"))
      ),
      None,
      None,
      None
    )

//    SelectExpr(
//      ArraySeq(ColumnExpr(Ident("name"))),
//      Seq(
//        TableOperator(Ident("e"))
//      ),
//      None,
//      None,
//      None
//    )

//    SelectExpr(
//      ArraySeq(StarExpr),
//      Seq(
//        LeftJoinOperator(
//          TableOperator(Ident("e")),
//          AliasOperator(TableOperator(Ident("e")), Ident("m")),
//          ComparisonExpr(ColumnExpr(Ident("e.m_id")), "=", ColumnExpr(Ident("m.e_id")))
//        )
//      ),
//      None,
//      None,
//      None
//    )

//    SelectExpr(
//      ArraySeq(
//        ColumnExpr(Ident("name")),
//        ApplyExpr(
//          Ident("table"),
//          Seq(
//            SelectExpr(
//              ArraySeq(ColumnExpr(Ident("name"))),
//              Seq(TableOperator(Ident("e"))),
//              Some(ComparisonExpr(ColumnExpr(Ident("outer.e_id")), "=", ColumnExpr(Ident("m_id")))),
//              None,
//              None
//            )
//          )
//        )
//      ),
//      Seq(AliasOperator(TableOperator(Ident("e")), Ident("outer"))),
//      Some(
//        ExistsExpr(
//          SelectExpr(
//            ArraySeq(StarExpr()),
//            Seq(TableOperator(Ident("e"))),
//            Some(ComparisonExpr(ColumnExpr(Ident("outer.e_id")), "=", ColumnExpr(Ident("m_id")))),
//            None,
//            None
//          )
//        )
//      ),
//      None,
//      None
//    )

  pprint.pprintln(eval(rewrite(sql)(db), Nil, AggregateMode.Return))

//  val input = "SELECT * FROM table"
