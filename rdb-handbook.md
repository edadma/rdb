# RDB Developer Handbook

*A cross-platform in-memory SQL database for JVM, JavaScript, and Native*

## Quick Start

### JavaScript/Node.js

```javascript
const { ConnectSQL } = require('@edadma/rdb');

const db = new ConnectSQL();

// Create and populate a table
db.execute(`
  CREATE TABLE users (
    id INT AUTO PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
`);

db.execute(`
  INSERT INTO users (name, email) VALUES 
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com')
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
    id UUID AUTO PRIMARY KEY,
    name TEXT NOT NULL,
    price NUMERIC(10,2)
  );
  
  INSERT INTO products (name, price) VALUES
    ('Laptop', 999.99),
    ('Mouse', 29.99);
  
  SELECT * FROM products WHERE price > 50;
""")

results.foreach(println)
```

## Core Concepts

### Database Instance
- **JavaScript**: `new ConnectSQL()` creates an in-memory database
- **Scala**: `new MemoryDB` creates an in-memory database
- Each instance is isolated and independent
- All data is stored in memory (no persistence)

### Execution Model
- **JavaScript**: `db.execute(sql)` returns array of result objects
- **Scala**: `executeSQL(sql)` returns sequence of Result objects
- Multiple statements can be executed in one call (semicolon-separated)
- Transactions are not explicitly supported (each statement is atomic)

## Data Types

### Numeric Types

```sql
-- Integer types
INT / INTEGER          -- 32-bit signed integer
BIGINT                 -- 64-bit signed integer  
DOUBLE                 -- Double-precision floating point
NUMERIC(precision, scale)  -- Fixed-precision decimal

-- Examples
CREATE TABLE numbers (
  small_int INT,
  big_int BIGINT,
  decimal_val NUMERIC(10,2),
  float_val DOUBLE
);
```

### Text and Binary

```sql
TEXT                   -- Variable-length string
UUID                   -- Universally unique identifier

-- Examples  
CREATE TABLE content (
  id UUID AUTO PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT
);
```

### Temporal

```sql
TIMESTAMP              -- Date and time values

-- Examples
CREATE TABLE events (
  id INT AUTO PRIMARY KEY,
  occurred_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### Structured Data

```sql
JSON                   -- JSON objects and arrays
ENUM('val1', 'val2')   -- Custom enumerated types

-- Examples
CREATE TYPE status AS ENUM ('active', 'inactive', 'pending');

CREATE TABLE records (
  id INT AUTO PRIMARY KEY,
  metadata JSON,
  status status
);
```

### Boolean

```sql
BOOLEAN                -- True/false values

CREATE TABLE flags (
  id INT AUTO PRIMARY KEY,
  is_enabled BOOLEAN DEFAULT FALSE
);
```

## Column Modifiers

### AUTO Increment

```sql
-- Automatically generates values
id INT AUTO PRIMARY KEY,           -- 1, 2, 3, ...
uuid_id UUID AUTO PRIMARY KEY,    -- Generated UUIDs
```

### Constraints

```sql
-- NOT NULL constraint
name TEXT NOT NULL,

-- PRIMARY KEY  
id INT AUTO PRIMARY KEY,

-- UNIQUE constraint
email TEXT UNIQUE,

-- DEFAULT values
created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
status TEXT DEFAULT 'pending',

-- FOREIGN KEY references
customer_id INT REFERENCES customers(id)
```

## DDL (Data Definition Language)

### CREATE TABLE

```sql
CREATE TABLE orders (
  id UUID AUTO PRIMARY KEY,
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status ENUM('pending', 'shipped', 'delivered') DEFAULT 'pending',
  metadata JSON,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### CREATE TYPE

```sql
-- Define custom enum types
CREATE TYPE priority AS ENUM ('low', 'medium', 'high', 'urgent');
CREATE TYPE user_role AS ENUM ('admin', 'moderator', 'user');

-- Use in table definitions
CREATE TABLE tasks (
  id INT AUTO PRIMARY KEY,
  title TEXT NOT NULL,
  priority priority DEFAULT 'medium'
);
```

### DROP TABLE

```sql
DROP TABLE table_name;
```

## DML (Data Manipulation Language)

### INSERT

```sql
-- Basic insert
INSERT INTO users (name, email) VALUES ('John', 'john@example.com');

-- Multiple rows
INSERT INTO products (name, price, category) VALUES
  ('Laptop', 999.99, 'Electronics'),
  ('Book', 19.99, 'Education'),
  ('Coffee', 4.50, 'Food');

-- With RETURNING clause  
INSERT INTO users (name, email) 
VALUES ('Jane', 'jane@example.com') 
RETURNING id;

-- Subquery insert
INSERT INTO archive_users (name, email)
SELECT name, email FROM users WHERE last_login < '2023-01-01';
```

### UPDATE

```sql
-- Basic update
UPDATE products SET price = 899.99 WHERE name = 'Laptop';

-- Multiple columns
UPDATE users 
SET email = 'newemail@example.com', 
    updated_at = CURRENT_TIMESTAMP 
WHERE id = 1;

-- Conditional updates
UPDATE orders 
SET status = 'shipped' 
WHERE status = 'pending' AND amount > 100;
```

### DELETE

```sql
-- Delete with condition
DELETE FROM users WHERE last_login < '2022-01-01';

-- Delete all rows
DELETE FROM temp_table;
```

## DQL (Data Query Language)

### Basic SELECT

```sql
-- All columns
SELECT * FROM users;

-- Specific columns
SELECT name, email FROM users;

-- Column aliases
SELECT name AS full_name, email AS email_address FROM users;

-- Calculated columns
SELECT name, price, price * 1.1 AS price_with_tax FROM products;
```

### WHERE Clause

```sql
-- Comparison operators
SELECT * FROM products WHERE price > 50;
SELECT * FROM products WHERE price BETWEEN 10 AND 100;
SELECT * FROM users WHERE name LIKE 'J%';
SELECT * FROM users WHERE email ILIKE '%GMAIL%';  -- Case-insensitive

-- Logical operators
SELECT * FROM products WHERE price > 50 AND category = 'Electronics';
SELECT * FROM users WHERE name LIKE 'A%' OR name LIKE 'B%';
SELECT * FROM products WHERE NOT category = 'Food';

-- NULL checks
SELECT * FROM users WHERE email IS NOT NULL;
SELECT * FROM users WHERE middle_name IS NULL;

-- IN operator
SELECT * FROM products WHERE category IN ('Electronics', 'Books');
SELECT * FROM users WHERE id NOT IN (1, 2, 3);
```

### ORDER BY

```sql
-- Single column
SELECT * FROM products ORDER BY price;
SELECT * FROM products ORDER BY price DESC;

-- Multiple columns
SELECT * FROM users ORDER BY last_name, first_name;

-- NULL handling
SELECT * FROM users ORDER BY email NULLS FIRST;
SELECT * FROM users ORDER BY email NULLS LAST;
```

### LIMIT and OFFSET

```sql
-- Pagination
SELECT * FROM products ORDER BY price LIMIT 10;
SELECT * FROM products ORDER BY price LIMIT 10 OFFSET 20;

-- Top N queries
SELECT * FROM products ORDER BY price DESC LIMIT 5;
```

### Aggregations

```sql
-- Basic aggregates
SELECT COUNT(*) FROM users;
SELECT COUNT(email) FROM users;  -- Excludes NULLs
SELECT SUM(price) FROM products;
SELECT AVG(price) FROM products;
SELECT MIN(price), MAX(price) FROM products;

-- GROUP BY
SELECT category, COUNT(*), AVG(price) 
FROM products 
GROUP BY category;

-- HAVING clause
SELECT category, AVG(price) as avg_price
FROM products 
GROUP BY category
HAVING AVG(price) > 50;
```

### Joins

```sql
-- INNER JOIN
SELECT o.id, o.amount, c.name 
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;

-- LEFT JOIN
SELECT c.name, COUNT(o.id) as order_count
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
GROUP BY c.name;

-- Multiple joins
SELECT o.id, c.name, p.name as product_name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id
INNER JOIN order_items oi ON o.id = oi.order_id
INNER JOIN products p ON oi.product_id = p.id;
```

### Subqueries

```sql
-- Scalar subquery
SELECT * FROM products 
WHERE price > (SELECT AVG(price) FROM products);

-- EXISTS
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o 
  WHERE o.customer_id = c.id AND o.amount > 100
);

-- IN subquery
SELECT * FROM products
WHERE category IN (
  SELECT category FROM products 
  GROUP BY category 
  HAVING COUNT(*) > 5
);
```

## Advanced Features

### JSON Operations

```sql
-- Store JSON data
INSERT INTO products (name, metadata) VALUES 
('Phone', '{"color": "black", "storage": "128GB", "features": ["wifi", "bluetooth"]}');

-- Query JSON (syntax varies by implementation)
SELECT * FROM products 
WHERE metadata->>'color' = 'black';

-- JSON functions
SELECT name, TYPEOF(metadata) FROM products;
```

### CASE Expressions

```sql
-- Simple CASE
SELECT name,
  CASE category
    WHEN 'Electronics' THEN 'Tech'
    WHEN 'Books' THEN 'Education'
    ELSE 'Other'
  END as category_group
FROM products;

-- Searched CASE
SELECT name, price,
  CASE 
    WHEN price < 20 THEN 'Cheap'
    WHEN price < 100 THEN 'Moderate' 
    ELSE 'Expensive'
  END as price_range
FROM products;
```

### Functions

```sql
-- Scalar functions
SELECT ABS(-42);                    -- 42
SELECT TYPEOF('hello');             -- 'text'

-- Table functions  
SELECT TABLE(SELECT * FROM users); -- Convert table to array

-- String operations
SELECT name || ' - ' || category FROM products;  -- Concatenation
```

## API Reference

### JavaScript API

#### ConnectSQL Class

```typescript
class ConnectSQL {
  constructor()
  execute(sql: string): any[]
}
```

#### Result Objects

```javascript
// CREATE TABLE result
{
  command: "create table",
  table: "users"
}

// INSERT result
{
  command: "insert", 
  result: { id: 1, name: "John" }  // Auto-generated and RETURNING values
}

// SELECT result  
{
  command: "select",
  result: [
    [1, "John", "john@example.com"],     // Row 1
    [2, "Jane", "jane@example.com"]      // Row 2
  ]
}

// UPDATE result
{
  command: "update",
  rows: 3  // Number of affected rows
}

// DELETE result  
{
  command: "delete", 
  rows: 1  // Number of deleted rows
}
```

### Scala API

#### Core Functions

```scala
// Create database
implicit val db: DB = new MemoryDB

// Execute SQL
def executeSQL(sql: String)(implicit db: DB): Seq[Result]
def executeQuery(query: String)(implicit db: DB): QueryResult
```

#### Result Types

```scala
sealed trait Result
case class QueryResult(table: TableValue) extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result  
case class CreateTableResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result
```

#### Value Types

```scala
// Extract values from Row
val row: Row = // ... from query result

// Type-safe extraction
val id: Int = row.getInt("id")
val name: String = row.getString("name") 
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// Direct access
val value: Value = row("column_name")
```

## Real-World Examples

### E-commerce System

```sql
-- Product catalog
CREATE TABLE categories (
  id INT AUTO PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT
);

CREATE TABLE products (
  id UUID AUTO PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  price NUMERIC(10,2) NOT NULL,
  category_id INT REFERENCES categories(id),
  attributes JSON,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Order management
CREATE TYPE order_status AS ENUM ('pending', 'processing', 'shipped', 'delivered', 'cancelled');

CREATE TABLE orders (
  id INT AUTO PRIMARY KEY,
  customer_email TEXT NOT NULL,
  status order_status DEFAULT 'pending',
  total_amount NUMERIC(10,2),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE order_items (
  id INT AUTO PRIMARY KEY,
  order_id INT REFERENCES orders(id),
  product_id UUID REFERENCES products(id),
  quantity INT NOT NULL,
  unit_price NUMERIC(10,2) NOT NULL
);

-- Sample data
INSERT INTO categories (name, description) VALUES
  ('Electronics', 'Electronic devices and accessories'),
  ('Books', 'Physical and digital books'),
  ('Clothing', 'Apparel and accessories');

INSERT INTO products (name, price, category_id, attributes) VALUES
  ('Laptop Pro', 1299.99, 1, '{"brand": "TechCorp", "warranty": "2 years"}'),
  ('Programming Guide', 49.99, 2, '{"pages": 450, "format": "paperback"}'),
  ('Cotton T-Shirt', 19.99, 3, '{"sizes": ["S", "M", "L", "XL"], "colors": ["red", "blue"]}');

-- Analytics queries
SELECT 
  c.name as category,
  COUNT(p.id) as product_count,
  AVG(p.price) as avg_price,
  MIN(p.price) as min_price,
  MAX(p.price) as max_price
FROM categories c
LEFT JOIN products p ON c.id = p.category_id
GROUP BY c.name
ORDER BY avg_price DESC;

-- Order summary with items
SELECT 
  o.id,
  o.customer_email,
  o.status,
  COUNT(oi.id) as item_count,
  SUM(oi.quantity * oi.unit_price) as calculated_total
FROM orders o
LEFT JOIN order_items oi ON o.id = oi.order_id
GROUP BY o.id, o.customer_email, o.status
ORDER BY o.created_at DESC;
```

### Analytics Dashboard

```sql
-- Time-series data
CREATE TABLE page_views (
  id INT AUTO PRIMARY KEY,
  page_url TEXT NOT NULL,
  user_id TEXT,
  session_id TEXT,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  metadata JSON
);

-- Daily aggregations
SELECT 
  DATE(timestamp) as view_date,
  COUNT(*) as total_views,
  COUNT(DISTINCT user_id) as unique_users,
  COUNT(DISTINCT session_id) as unique_sessions
FROM page_views
GROUP BY DATE(timestamp)
ORDER BY view_date DESC;

-- Top pages
SELECT 
  page_url,
  COUNT(*) as views,
  COUNT(DISTINCT user_id) as unique_viewers
FROM page_views
WHERE timestamp >= '2024-01-01'
GROUP BY page_url
ORDER BY views DESC
LIMIT 10;
```

### User Management

```sql
CREATE TYPE user_role AS ENUM ('admin', 'moderator', 'user', 'guest');

CREATE TABLE users (
  id UUID AUTO PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  email TEXT NOT NULL UNIQUE,
  role user_role DEFAULT 'user',
  profile JSON,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  last_login TIMESTAMP
);

CREATE TABLE user_sessions (
  id UUID AUTO PRIMARY KEY,
  user_id UUID REFERENCES users(id),
  token TEXT NOT NULL UNIQUE,
  expires_at TIMESTAMP NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- User activity report
SELECT 
  u.role,
  COUNT(*) as user_count,
  COUNT(CASE WHEN u.last_login >= '2024-01-01' THEN 1 END) as active_users,
  AVG(CASE WHEN u.last_login IS NOT NULL 
      THEN EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - u.last_login))/86400 
      END) as avg_days_since_login
FROM users u
GROUP BY u.role
ORDER BY user_count DESC;
```

## Performance Optimization

### Memory Considerations

```javascript
// Monitor memory usage in Node.js
const db = new ConnectSQL();

// Large dataset handling
const batchSize = 1000;
for (let i = 0; i < totalRecords; i += batchSize) {
  const batch = records.slice(i, i + batchSize);
  const values = batch.map(r => `('${r.name}', ${r.value})`).join(',');
  db.execute(`INSERT INTO table (name, value) VALUES ${values}`);
}
```

### Query Optimization

```sql
-- Use specific columns instead of SELECT *
SELECT id, name FROM products;  -- Better than SELECT *

-- Add WHERE clauses to limit results
SELECT * FROM large_table WHERE created_at >= '2024-01-01';

-- Use LIMIT for large result sets
SELECT * FROM products ORDER BY price DESC LIMIT 100;

-- Efficient aggregations
SELECT category, COUNT(*) FROM products GROUP BY category;
```

### Batch Operations

```sql
-- Bulk inserts are more efficient
INSERT INTO products (name, price) VALUES
  ('Product 1', 10.00),
  ('Product 2', 20.00),
  ('Product 3', 30.00);

-- Instead of multiple single inserts
-- INSERT INTO products (name, price) VALUES ('Product 1', 10.00);
-- INSERT INTO products (name, price) VALUES ('Product 2', 20.00);
-- INSERT INTO products (name, price) VALUES ('Product 3', 30.00);
```

## Troubleshooting

### Common Errors

**"table 'X' already exists"**
```sql
-- Check if table exists before creating
DROP TABLE IF EXISTS users;  -- Note: IF EXISTS not supported, use try/catch
CREATE TABLE users (...);
```

**"unknown column: 'X'"**
```sql
-- Verify column names match exactly (case-sensitive)
SELECT name, email FROM users;  -- Correct
-- SELECT Name, Email FROM users;  -- May fail if columns are lowercase
```

**"can't auto-convert 'X' to type 'Y'"**
```sql
-- Explicit type conversion needed
INSERT INTO products (price) VALUES ('19.99');  -- May fail
INSERT INTO products (price) VALUES (19.99);    -- Correct
```

**"aggregates not allowed here"**
```sql
-- Wrong: aggregate in WHERE
-- SELECT * FROM products WHERE price > AVG(price);

-- Correct: use subquery  
SELECT * FROM products WHERE price > (SELECT AVG(price) FROM products);
```

### Memory Issues

```javascript
// JavaScript: Monitor memory usage
console.log('Memory usage:', process.memoryUsage());

// Large datasets: process in chunks
function processLargeDataset(data) {
  const chunkSize = 10000;
  for (let i = 0; i < data.length; i += chunkSize) {
    const chunk = data.slice(i, i + chunkSize);
    // Process chunk
    if (i % 100000 === 0) {
      // Periodic cleanup hint
      if (global.gc) global.gc();
    }
  }
}
```

### Debugging Queries

```scala
// Scala: Enable query debugging
implicit val db: DB = new MemoryDB

// Print intermediate results
val results = executeSQL("SELECT COUNT(*) FROM users")
println(s"Result: $results")

// Check table contents
val tableContents = executeSQL("SELECT * FROM users LIMIT 5")
tableContents.foreach(println)
```

## Best Practices

### Schema Design

1. **Use appropriate data types**
   ```sql
   -- Good: Specific types
   CREATE TABLE orders (
     id UUID AUTO PRIMARY KEY,
     amount NUMERIC(10,2),
     created_at TIMESTAMP
   );
   
   -- Avoid: Everything as TEXT
   ```

2. **Define constraints**
   ```sql
   CREATE TABLE users (
     id INT AUTO PRIMARY KEY,
     email TEXT NOT NULL UNIQUE,
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   ```

3. **Use meaningful names**
   ```sql
   -- Good
   CREATE TABLE customer_orders (
     customer_id UUID,
     order_date TIMESTAMP
   );
   
   -- Avoid
   CREATE TABLE co (
     cid UUID,
     od TIMESTAMP  
   );
   ```

### Query Patterns

1. **Parameterize queries when possible**
   ```javascript
   // Simulate parameterization by building safe queries
   function findUserByEmail(email) {
     const safeEmail = email.replace(/'/g, "''"); // Basic SQL escaping
     return db.execute(`SELECT * FROM users WHERE email = '${safeEmail}'`);
   }
   ```

2. **Use meaningful aliases**
   ```sql
   SELECT 
     c.name as customer_name,
     o.amount as order_amount,
     o.created_at as order_date
   FROM customers c
   JOIN orders o ON c.id = o.customer_id;
   ```

3. **Structure complex queries clearly**
   ```sql
   SELECT 
     category,
     COUNT(*) as product_count,
     AVG(price) as average_price,
     SUM(CASE WHEN price > 100 THEN 1 ELSE 0 END) as expensive_count
   FROM products 
   WHERE created_at >= '2024-01-01'
   GROUP BY category
   HAVING COUNT(*) > 5
   ORDER BY average_price DESC;
   ```

### Application Integration

1. **Error handling**
   ```javascript
   function safeExecute(sql) {
     try {
       return db.execute(sql);
     } catch (error) {
       console.error('SQL Error:', error.message);
       return null;
     }
   }
   ```

2. **Connection management**
   ```javascript
   // Reuse database instances
   class DatabaseService {
     constructor() {
       this.db = new ConnectSQL();
     }
     
     query(sql) {
       return this.db.execute(sql);
     }
   }
   
   // Single instance for the application
   const dbService = new DatabaseService();
   ```

3. **Result processing**
   ```javascript
   function processSelectResult(result) {
     if (result[0]?.command === 'select') {
       return result[0].result.map(row => ({
         id: row[0],
         name: row[1], 
         email: row[2]
       }));
     }
     return [];
   }
   ```

## Platform-Specific Notes

### JavaScript/Node.js
- Works in browsers with bundlers (webpack, rollup, etc.)
- No external dependencies required
- Use `process.memoryUsage()` to monitor memory
- Consider Web Workers for large datasets in browsers

### JVM/Scala
- Thread-safe for concurrent access
- Can integrate with Spring Boot, Play Framework, etc.
- Use with Akka for high-concurrency scenarios
- Consider connection pooling patterns for multiple database instances

### Native
- Minimal runtime dependencies
- Compiles to native executables
- Ideal for CLI tools and embedded systems
- Consider memory constraints in embedded environments

---

**Need more help?** Check the [GitHub repository](https://github.com/edadma/rdb) for examples and issues.