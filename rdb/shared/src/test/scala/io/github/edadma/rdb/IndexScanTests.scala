package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IndexScanTests extends AnyFreeSpec with Matchers with Testing {

  "equality on unique index (primary key)" in {
    val t = query(
      """
        |CREATE TABLE users (
        |  id SERIAL,
        |  name TEXT,
        |  PRIMARY KEY (id)
        |);
        |INSERT INTO users (name) VALUES ('Alice');
        |INSERT INTO users (name) VALUES ('Bob');
        |INSERT INTO users (name) VALUES ('Charlie');
        |SELECT * FROM users WHERE id = 2;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(1) shouldBe TextValue("Bob")
  }

  "equality on non-unique index" in {
    val t = query(
      """
        |CREATE TABLE orders (
        |  id SERIAL,
        |  status TEXT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_status ON orders (status);
        |INSERT INTO orders (status) VALUES ('pending');
        |INSERT INTO orders (status) VALUES ('shipped');
        |INSERT INTO orders (status) VALUES ('pending');
        |INSERT INTO orders (status) VALUES ('delivered');
        |INSERT INTO orders (status) VALUES ('pending');
        |SELECT * FROM orders WHERE status = 'pending';
        |""".stripMargin,
    )
    t.data.length shouldBe 3
    t.data.foreach(row => row.data(1) shouldBe TextValue("pending"))
  }

  "BETWEEN on indexed column" in {
    val t = query(
      """
        |CREATE TABLE products (
        |  id SERIAL,
        |  price INT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_price ON products (price);
        |INSERT INTO products (price) VALUES (10);
        |INSERT INTO products (price) VALUES (20);
        |INSERT INTO products (price) VALUES (30);
        |INSERT INTO products (price) VALUES (40);
        |INSERT INTO products (price) VALUES (50);
        |SELECT * FROM products WHERE price BETWEEN 20 AND 40;
        |""".stripMargin,
    )
    t.data.length shouldBe 3
    val prices = t.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue).toSet
    prices shouldBe Set(20, 30, 40)
  }

  "compound WHERE: indexed col + non-indexed col" in {
    val t = query(
      """
        |CREATE TABLE items (
        |  id SERIAL,
        |  category TEXT,
        |  price INT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_cat ON items (category);
        |INSERT INTO items (category, price) VALUES ('A', 10);
        |INSERT INTO items (category, price) VALUES ('A', 50);
        |INSERT INTO items (category, price) VALUES ('B', 30);
        |INSERT INTO items (category, price) VALUES ('A', 20);
        |SELECT * FROM items WHERE category = 'A' AND price > 15;
        |""".stripMargin,
    )
    t.data.length shouldBe 2
    t.data.foreach(row => row.data(1) shouldBe TextValue("A"))
    val prices = t.data.map(_.data(2).asInstanceOf[NumberValue].value.intValue).toSet
    prices shouldBe Set(50, 20)
  }

  "no index available falls back to full scan" in {
    val t = query(
      """
        |CREATE TABLE things (
        |  id SERIAL,
        |  label TEXT,
        |  PRIMARY KEY (id)
        |);
        |INSERT INTO things (label) VALUES ('x');
        |INSERT INTO things (label) VALUES ('y');
        |INSERT INTO things (label) VALUES ('z');
        |SELECT * FROM things WHERE label = 'y';
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(1) shouldBe TextValue("y")
  }

  "empty result on non-existent key" in {
    val t = query(
      """
        |CREATE TABLE kv (
        |  k INT,
        |  v TEXT,
        |  PRIMARY KEY (k)
        |);
        |INSERT INTO kv (k, v) VALUES (1, 'one');
        |INSERT INTO kv (k, v) VALUES (2, 'two');
        |SELECT * FROM kv WHERE k = 999;
        |""".stripMargin,
    )
    t.data.length shouldBe 0
  }

  "NULL handling in index lookup" in {
    val t = query(
      """
        |CREATE TABLE nullable (
        |  id SERIAL,
        |  val INT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_val ON nullable (val);
        |INSERT INTO nullable (val) VALUES (10);
        |INSERT INTO nullable (val) VALUES (NULL);
        |INSERT INTO nullable (val) VALUES (20);
        |SELECT * FROM nullable WHERE val = 10;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(1) shouldBe NumberValue(DIntType, 10)
  }

  "UPDATE works on index-scanned rows" in {
    val r = results(
      """
        |CREATE TABLE updatable (
        |  id INT,
        |  name TEXT,
        |  PRIMARY KEY (id)
        |);
        |INSERT INTO updatable (id, name) VALUES (1, 'old');
        |INSERT INTO updatable (id, name) VALUES (2, 'keep');
        |UPDATE updatable SET name = 'new' WHERE id = 1;
        |SELECT name FROM updatable WHERE id = 1;
        |""".stripMargin,
    )
    val qr = r.collect { case QueryResult(t) => t }.last
    qr.data.length shouldBe 1
    qr.data.head.data(0) shouldBe TextValue("new")
  }

  "DELETE works on index-scanned rows" in {
    val r = results(
      """
        |CREATE TABLE deletable (
        |  id INT,
        |  name TEXT,
        |  PRIMARY KEY (id)
        |);
        |INSERT INTO deletable (id, name) VALUES (1, 'a');
        |INSERT INTO deletable (id, name) VALUES (2, 'b');
        |INSERT INTO deletable (id, name) VALUES (3, 'c');
        |DELETE FROM deletable WHERE id = 2;
        |SELECT * FROM deletable;
        |""".stripMargin,
    )
    val qr = r.collect { case QueryResult(t) => t }.last
    qr.data.length shouldBe 2
    val ids = qr.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue).toSet
    ids shouldBe Set(1, 3)
  }

}
