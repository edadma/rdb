package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DeleteUsingTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE customers (id SERIAL PRIMARY KEY, name TEXT NOT NULL, status TEXT NOT NULL);
      |CREATE TABLE orders (id SERIAL PRIMARY KEY, customer_id INT NOT NULL, amount INT NOT NULL);
      |INSERT INTO customers (name, status) VALUES ('Alice', 'active');
      |INSERT INTO customers (name, status) VALUES ('Bob', 'inactive');
      |INSERT INTO customers (name, status) VALUES ('Carol', 'inactive');
      |INSERT INTO customers (name, status) VALUES ('Dave', 'active');
      |INSERT INTO orders (customer_id, amount) VALUES (1, 100);
      |INSERT INTO orders (customer_id, amount) VALUES (2, 200);
      |INSERT INTO orders (customer_id, amount) VALUES (2, 300);
      |INSERT INTO orders (customer_id, amount) VALUES (3, 150);
      |INSERT INTO orders (customer_id, amount) VALUES (4, 250);
      |""".trim.stripMargin

  // ── Positive tests ──────────────────────────────────────────────────

  "DELETE ... USING" - {
    "deletes rows matching joined table condition" in {
      val session = setupSession(setup)
      given Session = session

      // Delete orders for inactive customers
      val deleteResults = executeSQL(
        "DELETE FROM orders USING customers WHERE orders.customer_id = customers.id AND customers.status = 'inactive';"
      )
      deleteResults.last match
        case DeleteResult(count) => count shouldBe 3 // Bob's 2 orders + Carol's 1 order
        case other               => fail(s"Expected DeleteResult, got $other")

      // Verify remaining orders
      val remaining = executeSQL("SELECT customer_id FROM orders ORDER BY id;").collect { case QueryResult(t) => t }.last
      remaining.data.length shouldBe 2
      remaining.data(0).data(0) shouldBe NumberValue(1) // Alice's order
      remaining.data(1).data(0) shouldBe NumberValue(4) // Dave's order
    }

    "deletes nothing when USING condition matches no rows" in {
      val session = setupSession(setup)
      given Session = session

      val deleteResults = executeSQL(
        "DELETE FROM orders USING customers WHERE orders.customer_id = customers.id AND customers.status = 'premium';"
      )
      deleteResults.last match
        case DeleteResult(count) => count shouldBe 0
        case other               => fail(s"Expected DeleteResult, got $other")

      // All orders still present
      val remaining = executeSQL("SELECT COUNT(*) AS cnt FROM orders;").collect { case QueryResult(t) => t }.last
      remaining.data(0).data(0).intValue shouldBe 5
    }

    "deletes all matching rows when no WHERE clause" in {
      val session = setupSession(setup)
      given Session = session

      // USING without WHERE — cross join deletes all orders (each order matched against every customer)
      val deleteResults = executeSQL("DELETE FROM orders USING customers;")
      deleteResults.last match
        case DeleteResult(count) => count shouldBe 5
        case other               => fail(s"Expected DeleteResult, got $other")

      val remaining = executeSQL("SELECT COUNT(*) AS cnt FROM orders;").collect { case QueryResult(t) => t }.last
      remaining.data(0).data(0).intValue shouldBe 0
    }

    "does not double-delete when multiple USING rows match same target row" in {
      val session = setupSession(
        """
          |CREATE TABLE tags (id SERIAL PRIMARY KEY, order_id INT NOT NULL, tag TEXT NOT NULL);
          |CREATE TABLE orders (id SERIAL PRIMARY KEY, amount INT NOT NULL);
          |INSERT INTO orders (amount) VALUES (100);
          |INSERT INTO orders (amount) VALUES (200);
          |INSERT INTO tags (order_id, tag) VALUES (1, 'urgent');
          |INSERT INTO tags (order_id, tag) VALUES (1, 'important');
          |INSERT INTO tags (order_id, tag) VALUES (2, 'normal');
          |""".trim.stripMargin
      )
      given Session = session

      // Order 1 has 2 tags, but should only be deleted once
      val deleteResults = executeSQL(
        "DELETE FROM orders USING tags WHERE orders.id = tags.order_id AND tags.tag IN ('urgent', 'important');"
      )
      deleteResults.last match
        case DeleteResult(count) => count shouldBe 1
        case other               => fail(s"Expected DeleteResult, got $other")

      val remaining = executeSQL("SELECT amount FROM orders;").collect { case QueryResult(t) => t }.last
      remaining.data.length shouldBe 1
      remaining.data(0).data(0) shouldBe NumberValue(200)
    }

    "works with RETURNING clause" in {
      val session = setupSession(setup)
      given Session = session

      val deleteResults = executeSQL(
        "DELETE FROM orders USING customers WHERE orders.customer_id = customers.id AND customers.status = 'inactive' RETURNING orders.id, orders.amount;"
      )
      deleteResults.last match
        case QueryResult(t) =>
          t.data.length shouldBe 3
          val amounts = t.data.map(r => r.data(1).intValue).sorted
          amounts shouldBe Seq(150, 200, 300)
        case other => fail(s"Expected QueryResult, got $other")
    }

    "works with multiple USING tables" in {
      val session = setupSession(
        """
          |CREATE TABLE categories (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE products (id SERIAL PRIMARY KEY, name TEXT NOT NULL, category_id INT NOT NULL);
          |CREATE TABLE order_items (id SERIAL PRIMARY KEY, product_id INT NOT NULL, qty INT NOT NULL);
          |INSERT INTO categories (name) VALUES ('electronics');
          |INSERT INTO categories (name) VALUES ('clothing');
          |INSERT INTO products (name, category_id) VALUES ('phone', 1);
          |INSERT INTO products (name, category_id) VALUES ('shirt', 2);
          |INSERT INTO products (name, category_id) VALUES ('laptop', 1);
          |INSERT INTO order_items (product_id, qty) VALUES (1, 5);
          |INSERT INTO order_items (product_id, qty) VALUES (2, 3);
          |INSERT INTO order_items (product_id, qty) VALUES (3, 2);
          |""".trim.stripMargin
      )
      given Session = session

      // Delete order items for electronics category using two USING tables
      val deleteResults = executeSQL(
        "DELETE FROM order_items USING products, categories WHERE order_items.product_id = products.id AND products.category_id = categories.id AND categories.name = 'electronics';"
      )
      deleteResults.last match
        case DeleteResult(count) => count shouldBe 2 // phone and laptop items
        case other               => fail(s"Expected DeleteResult, got $other")

      val remaining = executeSQL("SELECT product_id FROM order_items;").collect { case QueryResult(t) => t }.last
      remaining.data.length shouldBe 1
      remaining.data(0).data(0) shouldBe NumberValue(2) // shirt item
    }

    "leaves other tables unchanged" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL(
        "DELETE FROM orders USING customers WHERE orders.customer_id = customers.id AND customers.status = 'inactive';"
      )

      // Customers table should be untouched
      val customers = executeSQL("SELECT COUNT(*) AS cnt FROM customers;").collect { case QueryResult(t) => t }.last
      customers.data(0).data(0).intValue shouldBe 4
    }
  }

  // ── Negative tests ──────────────────────────────────────────────────

  "DELETE ... USING errors" - {
    "fails when USING references nonexistent table" in {
      val session = setupSession(setup)
      given Session = session

      an[Exception] should be thrownBy {
        executeSQL("DELETE FROM orders USING nonexistent WHERE orders.id = nonexistent.id;")
      }
    }

    "fails when WHERE references nonexistent column" in {
      val session = setupSession(setup)
      given Session = session

      an[Exception] should be thrownBy {
        executeSQL("DELETE FROM orders USING customers WHERE orders.customer_id = customers.nonexistent;")
      }
    }

    "fails when target table does not exist" in {
      val session = setupSession(setup)
      given Session = session

      an[Exception] should be thrownBy {
        executeSQL("DELETE FROM nonexistent USING customers WHERE nonexistent.id = customers.id;")
      }
    }
  }

  // ── Equivalence with subquery ───────────────────────────────────────

  "DELETE ... USING equivalence" - {
    "produces same result as equivalent IN subquery" in {
      // Run USING version
      val usingIds = {
        given Session = setupSession(setup)
        executeSQL(
          "DELETE FROM orders USING customers WHERE orders.customer_id = customers.id AND customers.status = 'inactive';"
        )
        executeSQL("SELECT id FROM orders ORDER BY id;").collect { case QueryResult(t) => t }.last
          .data.map(r => r.data(0).intValue)
      }

      // Run subquery version
      val subqueryIds = {
        given Session = setupSession(setup)
        executeSQL(
          "DELETE FROM orders WHERE customer_id IN (SELECT id FROM customers WHERE status = 'inactive');"
        )
        executeSQL("SELECT id FROM orders ORDER BY id;").collect { case QueryResult(t) => t }.last
          .data.map(r => r.data(0).intValue)
      }

      usingIds shouldBe subqueryIds
    }
  }
}
