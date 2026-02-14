# RDB - Relational Database

[![npm version](https://badge.fury.io/js/%40edadma%2Frdb.svg)](https://www.npmjs.com/package/@edadma/rdb)

A cross-platform in-memory relational database implementation written in Scala that compiles to JVM, JavaScript, and Native platforms. RDB provides a full SQL interface with support for tables, queries, joins, aggregations, and more.

## Overview

RDB is designed to provide a lightweight, embeddable SQL database for applications that need relational data operations without the overhead of a full database server. It's particularly useful for:

- **Testing and development** - Quick setup without external database dependencies
- **Client-side applications** - Running SQL queries in web browsers or Node.js
- **Data processing** - In-memory analytics and transformations
- **Embedded systems** - Native compilation for resource-constrained environments

The database supports standard SQL operations including CREATE TABLE, INSERT, SELECT, UPDATE, DELETE, with features like auto-increment columns, JSON data types, joins, subqueries, and aggregate functions.

## Installation

### JavaScript/Node.js

```bash
npm install @edadma/rdb
```

### Scala (SBT)

```scala
libraryDependencies += "io.github.edadma" %%% "rdb" % "0.0.29"
```

## Basic Usage

### JavaScript/TypeScript

```javascript
const { ConnectSQL } = require('@edadma/rdb');

const db = new ConnectSQL();

// Create a table
db.execute(`
  CREATE TABLE users (
    id INT AUTO PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT,
    created_at TIMESTAMP
  )
`);

// Insert data
db.execute(`
  INSERT INTO users (name, email, created_at) 
  VALUES ('John Doe', 'john@example.com', CURRENT_TIMESTAMP)
`);

// Query data
const results = db.execute('SELECT * FROM users');
console.log(results);
```

### Scala

```scala
import io.github.edadma.rdb.*

implicit val db: DB = new MemoryDB

val results = executeSQL("""
  CREATE TABLE products (
    id INT AUTO PRIMARY KEY,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    category TEXT
  );
  
  INSERT INTO products (name, price, category) VALUES
    ('Laptop', 999.99, 'Electronics'),
    ('Coffee', 4.50, 'Food'),
    ('Book', 19.99, 'Education');
  
  SELECT category, COUNT(*), AVG(price) 
  FROM products 
  GROUP BY category 
  ORDER BY category;
""")

results.foreach(println)
```

## Supported SQL Features

### Data Types

- **INT/INTEGER** - 32-bit integers with optional AUTO increment
- **BIGINT** - 64-bit integers
- **DOUBLE** - Double-precision floating point
- **NUMERIC(precision, scale)** - Fixed-precision decimal numbers
- **TEXT** - Variable-length strings
- **BOOLEAN** - True/false values
- **TIMESTAMP** - Date and time values
- **UUID** - Universally unique identifiers with AUTO generation
- **JSON** - Structured JSON objects and arrays
- **ENUM** - Custom enumerated types

### SQL Operations

#### DDL (Data Definition Language)
```sql
-- Create tables with constraints
CREATE TABLE orders (
  id UUID AUTO PRIMARY KEY,
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status ENUM('pending', 'shipped', 'delivered'),
  metadata JSON
);

-- Create custom types
CREATE TYPE status_type AS ENUM ('active', 'inactive', 'pending');

-- Drop tables
DROP TABLE orders;
```

#### DML (Data Manipulation Language)
```sql
-- Insert with explicit columns
INSERT INTO orders (customer_name, amount) 
VALUES ('Alice Smith', 149.99);

-- Insert with RETURNING clause
INSERT INTO orders (customer_name, amount) 
VALUES ('Bob Johnson', 75.50) 
RETURNING id;

-- Update records
UPDATE orders 
SET status = 'shipped' 
WHERE amount > 100;

-- Delete records
DELETE FROM orders 
WHERE status = 'delivered';
```

#### DQL (Data Query Language)
```sql
-- Basic queries with WHERE, ORDER BY, LIMIT
SELECT * FROM orders 
WHERE amount > 50 
ORDER BY amount DESC 
LIMIT 10;

-- Aggregations and GROUP BY
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders 
GROUP BY status
HAVING COUNT(*) > 5;

-- Joins
SELECT o.id, o.amount, c.name, c.email
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;

-- Subqueries and EXISTS
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o 
  WHERE o.customer_id = c.id AND o.amount > 100
);

-- JSON operations
SELECT * FROM products 
WHERE metadata->>'category' = 'electronics';
```

### Advanced Features

- **Window Functions** - ROW_NUMBER(), RANK(), etc.
- **Common Table Expressions (CTEs)** - WITH clauses
- **Case Expressions** - Conditional logic in SELECT
- **Pattern Matching** - LIKE and ILIKE operators
- **Set Operations** - UNION, INTERSECT, EXCEPT
- **Null Handling** - IS NULL, IS NOT NULL, COALESCE

## API Reference

### JavaScript/TypeScript API

```typescript
class ConnectSQL {
  constructor()
  
  /**
   * Execute one or more SQL statements
   * @param sql - SQL string (can contain multiple statements separated by ;)
   * @returns Array of result objects
   */
  execute(sql: string): any[]
}
```

#### Result Types

```javascript
// CREATE TABLE result
{
  command: "create table",
  table: "table_name"
}

// INSERT result  
{
  command: "insert",
  result: { id: 1, auto_column: "generated_value" }
}

// SELECT result
{
  command: "select", 
  result: [
    [value1, value2, ...], // Row 1
    [value3, value4, ...], // Row 2
    // ...
  ]
}
```

### Scala API

```scala
// Create database instance
implicit val db: DB = new MemoryDB

// Execute SQL and get results
val results: Seq[Result] = executeSQL("SELECT * FROM users")

// Result types
sealed trait Result
case class QueryResult(table: TableValue) extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result
```

## Examples

### E-commerce Database

```sql
-- Create product catalog
CREATE TABLE products (
  id UUID AUTO PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  price NUMERIC(10,2) NOT NULL,
  category TEXT,
  tags JSON,
  created_at TIMESTAMP
);

-- Create orders table  
CREATE TABLE orders (
  id INT AUTO PRIMARY KEY,
  product_id UUID NOT NULL,
  quantity INT NOT NULL,
  total_amount NUMERIC(10,2),
  order_date TIMESTAMP
);

-- Insert sample data
INSERT INTO products (name, price, category, tags) VALUES
  ('Wireless Headphones', 99.99, 'Electronics', '["bluetooth", "wireless", "audio"]'),
  ('Coffee Beans', 24.99, 'Food', '["organic", "fair-trade", "dark-roast"]'),
  ('Programming Book', 49.99, 'Books', '["programming", "technology", "education"]');

-- Complex query with joins and aggregations
SELECT 
  p.category,
  COUNT(o.id) as total_orders,
  SUM(o.total_amount) as total_revenue,
  AVG(o.total_amount) as avg_order_value
FROM products p
LEFT JOIN orders o ON p.id = o.product_id
WHERE p.created_at >= '2024-01-01'
GROUP BY p.category
HAVING total_orders > 0
ORDER BY total_revenue DESC;
```

### Analytics Example

```sql
-- Time-series analysis
WITH daily_sales AS (
  SELECT 
    DATE(order_date) as sale_date,
    SUM(total_amount) as daily_total,
    COUNT(*) as order_count
  FROM orders
  GROUP BY DATE(order_date)
),
moving_averages AS (
  SELECT 
    sale_date,
    daily_total,
    AVG(daily_total) OVER (
      ORDER BY sale_date 
      ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
    ) as seven_day_avg
  FROM daily_sales
)
SELECT * FROM moving_averages
ORDER BY sale_date DESC;
```

## Performance Considerations

- **Memory Usage** - All data is stored in memory; monitor usage for large datasets
- **Query Optimization** - Use indexes on frequently queried columns
- **Batch Operations** - Use bulk INSERT statements for better performance
- **Connection Pooling** - Reuse database instances when possible

## Platform-Specific Notes

### JavaScript/Node.js
- Compatible with Node.js 14+
- Works in browsers with bundlers (webpack, rollup, etc.)
- No external dependencies required

### JVM
- Requires Java 8+ or Scala 2.13+/3.x
- Can be used in Spring Boot, Play Framework, etc.
- Thread-safe for concurrent access

### Native
- Compiles to native executables with Scala Native
- Minimal runtime dependencies
- Ideal for CLI tools and embedded systems

## Testing

The project includes comprehensive test suites:

```bash
# Run tests for all platforms
sbt test

# Run JavaScript tests only  
sbt rdbJS/test

# Run JVM tests only
sbt rdbJVM/test

# Run Native tests only
sbt rdbNative/test
```

## Contributing

Contributions are welcome! Please follow these guidelines:

1. **Fork and Clone** - Fork the repository and clone your fork
2. **Create Branch** - Create a feature branch for your changes
3. **Write Tests** - Add tests for new functionality
4. **Follow Style** - Use the existing code formatting (scalafmt)
5. **Update Docs** - Update README and code comments as needed
6. **Submit PR** - Create a pull request with a clear description

### Development Setup

```bash
git clone https://github.com/edadma/rdb.git
cd rdb
sbt compile
sbt test
```

### Code Style

The project uses scalafmt for code formatting:

```bash
sbt scalafmtAll
```

## Contributors

- **Edward A. Maxedon, Sr.** - Original author and maintainer

## License

[ISC License](LICENSE) - see LICENSE file for details.

---

**Keywords:** SQL, database, in-memory, Scala, JavaScript, TypeScript, cross-platform, relational, embedded