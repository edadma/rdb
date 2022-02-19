package io.github.edadma.rdb

import pprint.pprintln

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

  pprintln(
    eval(OperatorExpr(e), Nil)
//    eval(
//      OperatorExpr(
//        FilterOperator(AliasOperator(e, "outer"), BinaryExpr(VariableExpr(None, Ident("e_id")), "IN", OperatorExpr(e)))
//      ),
//      Nil
//    )
  )

//  val myChannel = Channel[String] // Creates a Channel that receives Strings
//  val myVar = Var[Int](5) // Creates a Var containing the explicit value `5`
//  val myVal = Val[Int](myVar + 5) // Create a Val containing the sum of `myVar` + `5`
//
//  myVal.attach { newValue =>
//    println(s"myVal = $newValue")
//  }
//
//  println(myVal)
//
//  myVar := 10
