package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CreateTableTests extends AnyFreeSpec with Matchers with Testing {

  "PostgreSQL-style CREATE TABLE" - {
    
    "SERIAL primary key" in {
      test(
        """
          |CREATE TABLE users (
          | id SERIAL,
          | name TEXT NOT NULL,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO users (name) VALUES ('Alice');
          |INSERT INTO users (name) VALUES ('Bob');
          |SELECT * FROM users;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"users\")")
    }

    "BIGSERIAL primary key" in {
      test(
        """
          |CREATE TABLE orders (
          | order_id BIGSERIAL,
          | customer_name TEXT,
          | PRIMARY KEY (order_id)
          |);
          |INSERT INTO orders (customer_name) VALUES ('Customer A');
          |SELECT * FROM orders;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"orders\")")
    }

    "UUID with gen_random_uuid() default" in {
      test(
        """
          |CREATE TABLE sessions (
          | session_id UUID DEFAULT gen_random_uuid(),
          | user_id INT,
          | PRIMARY KEY (session_id)
          |);
          |INSERT INTO sessions (user_id) VALUES (123);
          |SELECT * FROM sessions;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"sessions\")")
    }

    "composite primary key" in {
      test(
        """
          |CREATE TABLE user_roles (
          | user_id INT NOT NULL,
          | role_id INT NOT NULL,
          | assigned_at TIMESTAMP,
          | PRIMARY KEY (user_id, role_id)
          |);
          |INSERT INTO user_roles (user_id, role_id) VALUES (1, 2);
          |SELECT * FROM user_roles;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"user_roles\")")
    }

    "named primary key constraint" in {
      test(
        """
          |CREATE TABLE products (
          | product_id SERIAL,
          | name TEXT NOT NULL,
          | price NUMERIC(10,2),
          | CONSTRAINT pk_products PRIMARY KEY (product_id)
          |);
          |INSERT INTO products (name, price) VALUES ('Widget', 19.99);
          |SELECT * FROM products;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"products\")")
    }

    "unique constraints" in {
      test(
        """
          |CREATE TABLE customers (
          | id SERIAL,
          | email TEXT NOT NULL,
          | username TEXT,
          | PRIMARY KEY (id),
          | UNIQUE (email),
          | CONSTRAINT uk_customers_username UNIQUE (username)
          |);
          |INSERT INTO customers (email, username) VALUES ('test@example.com', 'testuser');
          |SELECT * FROM customers;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"customers\")")
    }

    "foreign key constraints" in {
      test(
        """
          |CREATE TABLE departments (
          | dept_id SERIAL,
          | dept_name TEXT NOT NULL,
          | PRIMARY KEY (dept_id)
          |);
          |
          |CREATE TABLE employees (
          | emp_id SERIAL,
          | name TEXT NOT NULL,
          | dept_id INT,
          | PRIMARY KEY (emp_id),
          | FOREIGN KEY (dept_id) REFERENCES departments(dept_id)
          |);
          |
          |INSERT INTO departments (dept_name) VALUES ('Engineering');
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |SELECT * FROM employees;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"departments\")")
    }

    "composite foreign key" in {
      test(
        """
          |CREATE TABLE companies (
          | company_id SERIAL,
          | region_id INT,
          | company_name TEXT,
          | PRIMARY KEY (company_id, region_id)
          |);
          |
          |CREATE TABLE branches (
          | branch_id SERIAL,
          | company_id INT NOT NULL,
          | region_id INT NOT NULL,
          | branch_name TEXT,
          | PRIMARY KEY (branch_id),
          | CONSTRAINT fk_branches_company FOREIGN KEY (company_id, region_id) REFERENCES companies(company_id, region_id)
          |);
          |
          |INSERT INTO companies (region_id, company_name) VALUES (1, 'TechCorp');
          |INSERT INTO branches (company_id, region_id, branch_name) VALUES (1, 1, 'Main Branch');
          |SELECT * FROM branches;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"companies\")")
    }

    "mixed data types with constraints" in {
      test(
        """
          |CREATE TABLE audit_log (
          | log_id BIGSERIAL,
          | session_id UUID DEFAULT gen_random_uuid(),
          | user_id INT NOT NULL,
          | "action" TEXT NOT NULL,
          | details JSON,
          | created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          | is_deleted BOOLEAN,
          | PRIMARY KEY (log_id),
          | UNIQUE (session_id)
          |);
          |
          |INSERT INTO audit_log (user_id, "action", details) 
          |VALUES (123, 'login', '{"ip": "192.168.1.1", "user_agent": "Chrome"}');
          |
          |SELECT log_id, user_id, "action" FROM audit_log;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"audit_log\")")
    }
  }

  "inline PRIMARY KEY on column" - {

    "single column inline PRIMARY KEY" in {
      test(
        """
          |CREATE TABLE t1 (
          | id SERIAL PRIMARY KEY,
          | name TEXT NOT NULL
          |);
          |INSERT INTO t1 (name) VALUES ('Alice');
          |SELECT * FROM t1;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"t1\")")
    }

    "inline PRIMARY KEY implies NOT NULL" in {
      // inserting NULL into a PK column should fail
      val ex = suppressStderr {
        intercept[Exception] {
          test(
            """
              |CREATE TABLE t2 (
              | id INT PRIMARY KEY,
              | name TEXT
              |);
              |INSERT INTO t2 (id, name) VALUES (NULL, 'test');
              |""".trim.stripMargin
          )
        }
      }
      ex.getMessage should include("is required")
    }

    "column-level + table-level PRIMARY KEY conflict" in {
      val ex = suppressStderr {
        intercept[Exception] {
          test(
            """
              |CREATE TABLE t3 (
              | id INT PRIMARY KEY,
              | name TEXT,
              | PRIMARY KEY (id)
              |);
              |""".trim.stripMargin
        )
        }
      }
      ex.getMessage should include("column-level and table-level PRIMARY KEY")
    }
  }

  "any-order column constraints" - {

    "DEFAULT before NOT NULL" in {
      test(
        """
          |CREATE TABLE t4 (
          | id SERIAL,
          | x INT DEFAULT 0 NOT NULL,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO t4 (id) VALUES (1);
          |SELECT x FROM t4;
          |""".trim.stripMargin
      ) should include("0")
    }

    "UNIQUE DEFAULT CHECK in mixed order" in {
      test(
        """
          |CREATE TABLE t5 (
          | id SERIAL PRIMARY KEY,
          | x INT UNIQUE DEFAULT 5 CHECK (x > 0)
          |);
          |INSERT INTO t5 (id) VALUES (1);
          |SELECT x FROM t5;
          |""".trim.stripMargin
      ) should include("5")
    }

    "NOT NULL after DEFAULT and UNIQUE" in {
      test(
        """
          |CREATE TABLE t6 (
          | id SERIAL PRIMARY KEY,
          | code TEXT DEFAULT 'X' UNIQUE NOT NULL
          |);
          |INSERT INTO t6 (id) VALUES (1);
          |SELECT code FROM t6;
          |""".trim.stripMargin
      ) should include("X")
    }
  }

  "NUMERIC/DECIMAL without full precision" - {

    "bare NUMERIC" in {
      test(
        """
          |CREATE TABLE t7 (
          | id SERIAL PRIMARY KEY,
          | val NUMERIC
          |);
          |INSERT INTO t7 (val) VALUES (42);
          |SELECT val FROM t7;
          |""".trim.stripMargin
      ) should include("42")
    }

    "NUMERIC with precision only" in {
      test(
        """
          |CREATE TABLE t8 (
          | id SERIAL PRIMARY KEY,
          | val NUMERIC(10)
          |);
          |INSERT INTO t8 (val) VALUES (12345);
          |SELECT val FROM t8;
          |""".trim.stripMargin
      ) should include("12345")
    }

    "bare DECIMAL" in {
      test(
        """
          |CREATE TABLE t9 (
          | id SERIAL PRIMARY KEY,
          | val DECIMAL
          |);
          |INSERT INTO t9 (val) VALUES (99);
          |SELECT val FROM t9;
          |""".trim.stripMargin
      ) should include("99")
    }

    "DECIMAL with precision only" in {
      test(
        """
          |CREATE TABLE t10 (
          | id SERIAL PRIMARY KEY,
          | val DECIMAL(8)
          |);
          |INSERT INTO t10 (val) VALUES (67890);
          |SELECT val FROM t10;
          |""".trim.stripMargin
      ) should include("67890")
    }
  }

  "PostgreSQL compatibility features" - {

    "multiple table creation in single statement" in {
      test(
        """
          |CREATE TABLE categories (
          | id SERIAL,
          | name TEXT NOT NULL,
          | PRIMARY KEY (id)
          |);
          |
          |CREATE TABLE items (
          | id SERIAL,
          | category_id INT,
          | name TEXT NOT NULL,
          | price NUMERIC(8,2),
          | PRIMARY KEY (id),
          | FOREIGN KEY (category_id) REFERENCES categories(id)
          |);
          |
          |INSERT INTO categories (name) VALUES ('Electronics');
          |INSERT INTO items (category_id, name, price) VALUES (1, 'Laptop', 999.99);
          |SELECT i.name, c.name FROM items i JOIN categories c ON i.category_id = c.id;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"categories\")")
    }

    "constraint validation" in {
      test(
        """
          |CREATE TABLE test_constraints (
          | id SERIAL,
          | unique_col TEXT,
          | PRIMARY KEY (id),
          | UNIQUE (unique_col)
          |);
          |INSERT INTO test_constraints (unique_col) VALUES ('test1');
          |SELECT * FROM test_constraints;
          |""".trim.stripMargin
      ) should include("CreateTableResult(\"test_constraints\")")
    }
  }
}