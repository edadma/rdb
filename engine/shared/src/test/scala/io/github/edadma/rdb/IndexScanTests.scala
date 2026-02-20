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

  // --- Composite index scan tests ---

  "composite full match on 3-column non-unique index" in {
    val t = query(
      """
        |CREATE TABLE events (
        |  id SERIAL,
        |  year INT,
        |  month INT,
        |  day INT,
        |  label TEXT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_ymd ON events (year, month, day);
        |INSERT INTO events (year, month, day, label) VALUES (2024, 1, 15, 'a');
        |INSERT INTO events (year, month, day, label) VALUES (2024, 1, 20, 'b');
        |INSERT INTO events (year, month, day, label) VALUES (2024, 2, 15, 'c');
        |INSERT INTO events (year, month, day, label) VALUES (2025, 1, 15, 'd');
        |SELECT label FROM events WHERE year = 2024 AND month = 1 AND day = 15;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(0) shouldBe TextValue("a")
  }

  "composite prefix match 2-of-3 columns" in {
    val t = query(
      """
        |CREATE TABLE events2 (
        |  id SERIAL,
        |  year INT,
        |  month INT,
        |  day INT,
        |  label TEXT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_ymd2 ON events2 (year, month, day);
        |INSERT INTO events2 (year, month, day, label) VALUES (2024, 1, 10, 'a');
        |INSERT INTO events2 (year, month, day, label) VALUES (2024, 1, 20, 'b');
        |INSERT INTO events2 (year, month, day, label) VALUES (2024, 2, 10, 'c');
        |INSERT INTO events2 (year, month, day, label) VALUES (2025, 1, 10, 'd');
        |SELECT label FROM events2 WHERE year = 2024 AND month = 1;
        |""".stripMargin,
    )
    t.data.length shouldBe 2
    val labels = t.data.map(_.data(0).asInstanceOf[TextValue].s).toSet
    labels shouldBe Set("a", "b")
  }

  "composite prefix match 1-of-3 columns falls back to single-column or scan" in {
    val t = query(
      """
        |CREATE TABLE events3 (
        |  id SERIAL,
        |  year INT,
        |  month INT,
        |  day INT,
        |  label TEXT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_ymd3 ON events3 (year, month, day);
        |INSERT INTO events3 (year, month, day, label) VALUES (2024, 1, 10, 'a');
        |INSERT INTO events3 (year, month, day, label) VALUES (2024, 2, 20, 'b');
        |INSERT INTO events3 (year, month, day, label) VALUES (2025, 1, 10, 'c');
        |SELECT label FROM events3 WHERE year = 2024;
        |""".stripMargin,
    )
    t.data.length shouldBe 2
    val labels = t.data.map(_.data(0).asInstanceOf[TextValue].s).toSet
    labels shouldBe Set("a", "b")
  }

  "composite with residual filter on non-indexed column" in {
    val t = query(
      """
        |CREATE TABLE sales (
        |  id SERIAL,
        |  region TEXT,
        |  year INT,
        |  amount INT,
        |  PRIMARY KEY (id)
        |);
        |CREATE INDEX idx_ry ON sales (region, year);
        |INSERT INTO sales (region, year, amount) VALUES ('east', 2024, 100);
        |INSERT INTO sales (region, year, amount) VALUES ('east', 2024, 500);
        |INSERT INTO sales (region, year, amount) VALUES ('east', 2025, 200);
        |INSERT INTO sales (region, year, amount) VALUES ('west', 2024, 300);
        |SELECT amount FROM sales WHERE region = 'east' AND year = 2024 AND amount > 200;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(0).asInstanceOf[NumberValue].value.intValue shouldBe 500
  }

  "composite full match on unique index" in {
    val t = query(
      """
        |CREATE TABLE coords (
        |  x INT,
        |  y INT,
        |  label TEXT
        |);
        |CREATE UNIQUE INDEX idx_xy ON coords (x, y);
        |INSERT INTO coords (x, y, label) VALUES (1, 2, 'a');
        |INSERT INTO coords (x, y, label) VALUES (1, 3, 'b');
        |INSERT INTO coords (x, y, label) VALUES (2, 2, 'c');
        |SELECT label FROM coords WHERE x = 1 AND y = 2;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(0) shouldBe TextValue("a")
  }

  "composite prefix match on unique index" in {
    val t = query(
      """
        |CREATE TABLE coords2 (
        |  x INT,
        |  y INT,
        |  label TEXT
        |);
        |CREATE UNIQUE INDEX idx_xy2 ON coords2 (x, y);
        |INSERT INTO coords2 (x, y, label) VALUES (1, 2, 'a');
        |INSERT INTO coords2 (x, y, label) VALUES (1, 3, 'b');
        |INSERT INTO coords2 (x, y, label) VALUES (2, 2, 'c');
        |SELECT label FROM coords2 WHERE x = 1;
        |""".stripMargin,
    )
    t.data.length shouldBe 2
    val labels = t.data.map(_.data(0).asInstanceOf[TextValue].s).toSet
    labels shouldBe Set("a", "b")
  }

  "composite conjuncts in reverse order" in {
    val t = query(
      """
        |CREATE TABLE pairs (
        |  a INT,
        |  b INT,
        |  val TEXT
        |);
        |CREATE INDEX idx_ab ON pairs (a, b);
        |INSERT INTO pairs (a, b, val) VALUES (1, 10, 'x');
        |INSERT INTO pairs (a, b, val) VALUES (1, 20, 'y');
        |INSERT INTO pairs (a, b, val) VALUES (2, 10, 'z');
        |SELECT val FROM pairs WHERE b = 10 AND a = 1;
        |""".stripMargin,
    )
    t.data.length shouldBe 1
    t.data.head.data(0) shouldBe TextValue("x")
  }

  "composite gap in prefix becomes residual" in {
    val t = query(
      """
        |CREATE TABLE triples (
        |  a INT,
        |  b INT,
        |  c INT,
        |  label TEXT
        |);
        |CREATE INDEX idx_abc ON triples (a, b, c);
        |INSERT INTO triples (a, b, c, label) VALUES (1, 10, 100, 'hit');
        |INSERT INTO triples (a, b, c, label) VALUES (1, 20, 100, 'also');
        |INSERT INTO triples (a, b, c, label) VALUES (1, 10, 200, 'miss');
        |INSERT INTO triples (a, b, c, label) VALUES (2, 10, 100, 'nope');
        |SELECT label FROM triples WHERE a = 1 AND c = 100;
        |""".stripMargin,
    )
    // Gap at b: only prefix a=1 used, c=100 becomes residual filter
    t.data.length shouldBe 2
    val labels = t.data.map(_.data(0).asInstanceOf[TextValue].s).toSet
    labels shouldBe Set("hit", "also")
  }

}
