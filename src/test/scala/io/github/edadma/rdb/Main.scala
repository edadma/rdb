package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import pprint._

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
    db.create("e", Seq(ColumnSpec("e_id", NumberType), ColumnSpec("name", TextType), ColumnSpec("m_id", NumberType)))

  e.bulkInsert(
    Seq("e_id", "name", "m_id"),
    Seq(
      Seq(NumberValue(1), TextValue("emp1"), NumberValue(2)),
      Seq(NumberValue(2), TextValue("mgr1"), NumberValue(3)),
      Seq(NumberValue(3), TextValue("mgr2"), NULL),
      Seq(NumberValue(4), TextValue("emp2"), NumberValue(3)),
      Seq(NumberValue(5), TextValue("emp3"), NumberValue(3))
    )
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
// WHERE EXISTS(SELECT * FROM e )
  val input =
//    """
//      |SELECT name, TABLE(SELECT * FROM e WHERE mgr.e_id = m_id) FROM e mgr WHERE EXISTS (SELECT * FROM e WHERE mgr.e_id = m_id)
//      |""".trim.stripMargin
    """
      |SELECT -e_id FROM e OFFSET 2 LIMIT 2
      |""".trim.stripMargin
  val ast = SQLParser.parseQuery(input)

  pprintln(ast)
  pprintln(eval(rewrite(ast)(db), Nil, AggregateMode.Return))
