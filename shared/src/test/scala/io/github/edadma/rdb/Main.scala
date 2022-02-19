package io.github.edadma.rdb

import reactify._
import pprint.pprintln

object Main extends App:

  val db = MemoryDB("test")
  val t = db.create("t", Seq(ColumnSpec("t_id", NumberType), ColumnSpec("t_text", StringType)))

  t.bulkInsert(
    Seq("t_id", "t_text"),
    Seq(
      Seq(NumberValue(1), StringValue("t1")),
      Seq(NumberValue(2), StringValue("t2"))
    )
  )

  val u = db.create("u", Seq(ColumnSpec("u_id", NumberType), ColumnSpec("u_text", StringType)))

  u.bulkInsert(
    Seq("u_id", "u_text"),
    Seq(
      Seq(NumberValue(1), StringValue("u1")),
      Seq(NumberValue(2), StringValue("u2")),
      Seq(NumberValue(3), StringValue("u3"))
    )
  )

  pprintln(
    FilterOperator(CrossOperator(t, u), BinaryExpr(VariableExpr(None, Ident("a")), ">", NumberExpr(3)))
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
