package io.github.edadma.rdb

import reactify._

object Main extends App:

  val db = new MemoryDB("test")
  val t = db.create("t", RowMeta(Seq(ColumnMeta("a", NumberType))))

  t.bulkInsert(
    Seq("a"),
    Seq(
      Seq(NumberValue(3))
    )
  )

  println(CollectStep(ScanStep(t)).value)

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
