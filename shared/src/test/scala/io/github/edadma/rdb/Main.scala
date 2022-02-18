package io.github.edadma.rdb

import reactify._

object Main extends App:

  val db = new MemoryDB("test")
  val t = db.create("t", Seq(ColumnSpec("a", NumberType), ColumnSpec("b", StringType)))

  t.bulkInsert(
    Seq("a", "b"),
    Seq(
      Seq(NumberValue(3), StringValue("three")),
      Seq(NumberValue(4), StringValue("four"))
    )
  )

  println(
    CollectStep(
      FilterStep(t, BinaryExpr(VariableExpr(None, Ident("a")), ">", NumberExpr(3)), () => Nil)
    ).value
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
