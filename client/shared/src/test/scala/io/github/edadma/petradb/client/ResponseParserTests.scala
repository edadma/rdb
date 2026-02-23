package io.github.edadma.petradb.client

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.petradb.*
import io.github.edadma.dal.IntType as DIntType

class ResponseParserTests extends AnyFreeSpec with Matchers:

  "ResponseParser" - {

    "parses select result" in {
      val json    = """[{"command":"select","fields":[{"name":"id","dataType":"integer"},{"name":"name","dataType":"text"}],"rows":[{"id":1,"name":"Alice"}]}]"""
      val results = ResponseParser.parseResponse(json)
      results should have length 1
      val table = results.head.asInstanceOf[QueryResult].table
      table.data should have length 1
      table.data.head("id") shouldBe NumberValue(DIntType, 1)
      table.data.head("name") shouldBe TextValue("Alice")
    }

    "parses update result" in {
      val results = ResponseParser.parseResponse("""[{"command":"update","rowCount":3}]""")
      results.head shouldBe UpdateResult(3)
    }

    "parses delete result" in {
      val results = ResponseParser.parseResponse("""[{"command":"delete","rowCount":2}]""")
      results.head shouldBe DeleteResult(2)
    }

    "parses create table result" in {
      val results = ResponseParser.parseResponse("""[{"command":"create table","table":"users"}]""")
      results.head shouldBe CreateTableResult("users")
    }

    "parses create index result" in {
      val results = ResponseParser.parseResponse("""[{"command":"create index","index":"idx_name"}]""")
      results.head shouldBe CreateIndexResult("idx_name")
    }

    "parses create type result" in {
      val results = ResponseParser.parseResponse("""[{"command":"create type","type":"mood"}]""")
      results.head shouldBe CreateTypeResult("mood")
    }

    "parses null values" in {
      val json  = """[{"command":"select","fields":[{"name":"x","dataType":"text"}],"rows":[{"x":null}]}]"""
      val table = ResponseParser.parseResponse(json).head.asInstanceOf[QueryResult].table
      table.data.head("x") shouldBe NullValue()
    }

    "parses boolean values" in {
      val json  = """[{"command":"select","fields":[{"name":"b","dataType":"boolean"}],"rows":[{"b":true}]}]"""
      val table = ResponseParser.parseResponse(json).head.asInstanceOf[QueryResult].table
      table.data.head("b") shouldBe BooleanValue(true)
    }

    "parses multiple results" in {
      val json    = """[{"command":"create table","table":"t"},{"command":"update","rowCount":5}]"""
      val results = ResponseParser.parseResponse(json)
      results should have length 2
      results(0) shouldBe CreateTableResult("t")
      results(1) shouldBe UpdateResult(5)
    }

    "parses array row mode" in {
      val json    = """[{"command":"select","fields":[{"name":"id","dataType":"integer"}],"rows":[[42]]}]"""
      val results = ResponseParser.parseResponse(json, rowMode = "array")
      val table   = results.head.asInstanceOf[QueryResult].table
      table.data.head.data.head shouldBe NumberValue(DIntType, 42)
    }

    "parses insert result" in {
      val json    = """[{"command":"insert","result":{"id":1},"fields":[{"name":"id","dataType":"serial"}],"rows":[{"id":1}]}]"""
      val results = ResponseParser.parseResponse(json)
      val ins     = results.head.asInstanceOf[InsertResult]
      ins.obj("id") shouldBe NumberValue(DIntType, 1)
    }
  }
