-- ============================================================
-- PetraDB Sample Database: Bookstore
-- ============================================================
-- Demonstrates: enums, serial/UUID PKs, foreign keys,
-- numeric, timestamps, indexes, and rich sample data.
-- ============================================================

-- ── Enum Types ───────────────────────────────────────────────

CREATE TYPE order_status AS ENUM ('pending', 'confirmed', 'shipped', 'delivered', 'cancelled');
CREATE TYPE book_format  AS ENUM ('hardcover', 'paperback', 'ebook', 'audiobook');
CREATE TYPE genre        AS ENUM ('fiction', 'non-fiction', 'mystery', 'science-fiction', 'biography', 'history', 'fantasy', 'thriller', 'romance', 'self-help');

-- ── Authors ──────────────────────────────────────────────────

CREATE TABLE authors (
                         id         SERIAL PRIMARY KEY,
                         first_name TEXT NOT NULL,
                         last_name  TEXT NOT NULL,
                         birth_year INTEGER,
                         country    TEXT
);

INSERT INTO authors (first_name, last_name, birth_year, country) VALUES
                                                                     ('Ursula',    'Le Guin',      1929, 'USA'),
                                                                     ('George',    'Orwell',       1903, 'UK'),
                                                                     ('Toni',      'Morrison',     1931, 'USA'),
                                                                     ('Haruki',    'Murakami',     1949, 'Japan'),
                                                                     ('Chimamanda','Ngozi Adichie',1977, 'Nigeria'),
                                                                     ('Frank',     'Herbert',      1920, 'USA'),
                                                                     ('Agatha',    'Christie',     1890, 'UK'),
                                                                     ('Gabriel',   'García Márquez',1927,'Colombia'),
                                                                     ('Margaret',  'Atwood',       1939, 'Canada'),
                                                                     ('Kazuo',     'Ishiguro',     1954, 'UK');

-- ── Books ────────────────────────────────────────────────────

CREATE TABLE books (
                       id           SERIAL PRIMARY KEY,
                       author_id    INTEGER NOT NULL REFERENCES authors(id),
                       title        TEXT NOT NULL,
                       isbn         TEXT UNIQUE,
                       genre        genre NOT NULL,
                       format       book_format NOT NULL DEFAULT 'paperback',
                       price        NUMERIC(8,2) NOT NULL,
                       stock        INTEGER NOT NULL DEFAULT 0,
                       published    DATE,
                       description  TEXT
);

INSERT INTO books (author_id, title, isbn, genre, format, price, stock, published) VALUES
                                                                                       (1,  'The Left Hand of Darkness',  '978-0441478125', 'science-fiction', 'paperback',  12.99,  23, '1969-03-01'),
                                                                                       (1,  'The Dispossessed',           '978-0061054884', 'science-fiction', 'paperback',  13.99,  15, '1974-05-01'),
                                                                                       (2,  'Nineteen Eighty-Four',       '978-0451524935', 'fiction',         'paperback',  10.99,  42, '1949-06-08'),
                                                                                       (2,  'Animal Farm',                '978-0451526342', 'fiction',         'paperback',   8.99,  38, '1945-08-17'),
                                                                                       (3,  'Beloved',                    '978-1400033416', 'fiction',         'paperback',  14.99,  11, '1987-09-02'),
                                                                                       (3,  'Song of Solomon',            '978-1400033423', 'fiction',         'paperback',  13.49,   8, '1977-09-01'),
                                                                                       (4,  'Norwegian Wood',             '978-0375704024', 'fiction',         'paperback',  14.99,  19, '1987-09-04'),
                                                                                       (4,  'Kafka on the Shore',         '978-1400079278', 'fiction',         'paperback',  15.99,  14, '2002-09-12'),
                                                                                       (5,  'Half of a Yellow Sun',       '978-1400095209', 'fiction',         'hardcover',  22.99,   7, '2006-09-12'),
                                                                                       (5,  'Americanah',                 '978-0307455925', 'fiction',         'paperback',  15.99,  16, '2013-05-14'),
                                                                                       (6,  'Dune',                       '978-0441013593', 'science-fiction', 'paperback',  16.99,  31, '1965-08-01'),
                                                                                       (6,  'Dune Messiah',               '978-0593098233', 'science-fiction', 'paperback',  14.99,  18, '1969-01-01'),
                                                                                       (7,  'Murder on the Orient Express','978-0062693662','mystery',         'paperback',  11.99,  27, '1934-01-01'),
                                                                                       (7,  'And Then There Were None',   '978-0062073488', 'mystery',         'paperback',  10.99,  33, '1939-11-06'),
                                                                                       (8,  'One Hundred Years of Solitude','978-0060883287','fiction',        'paperback',  15.99,  21, '1967-05-30'),
                                                                                       (9,  'The Handmaid''s Tale',       '978-0385490818', 'fiction',        'paperback',  13.99,  29, '1985-09-01'),
                                                                                       (9,  'Oryx and Crake',             '978-0385721677', 'science-fiction', 'paperback',  14.99,  12, '2003-05-06'),
                                                                                       (10, 'Never Let Me Go',            '978-1400078776', 'fiction',         'paperback',  13.99,  17, '2005-03-03'),
                                                                                       (10, 'The Remains of the Day',     '978-0679731726', 'fiction',         'hardcover',  19.99,   9, '1989-05-01');

-- ── Indexes ──────────────────────────────────────────────────

CREATE INDEX books_author_idx ON books (author_id);
CREATE INDEX books_genre_idx  ON books (genre);

-- ── Customers ────────────────────────────────────────────────

CREATE TABLE customers (
                           id         SERIAL PRIMARY KEY,
                           email      TEXT NOT NULL UNIQUE,
                           first_name TEXT NOT NULL,
                           last_name  TEXT NOT NULL,
                           city       TEXT,
                           country    TEXT,
                           joined_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO customers (email, first_name, last_name, city, country) VALUES
                                                                        ('alice@example.com',   'Alice',   'Chen',      'San Francisco', 'USA'),
                                                                        ('bob@example.com',     'Bob',     'Okafor',    'London',        'UK'),
                                                                        ('clara@example.com',   'Clara',   'Dubois',    'Paris',         'France'),
                                                                        ('dimitri@example.com', 'Dimitri', 'Petrov',    'Berlin',        'Germany'),
                                                                        ('elena@example.com',   'Elena',   'Santos',    'São Paulo',     'Brazil'),
                                                                        ('farouk@example.com',  'Farouk',  'Hassan',    'Cairo',         'Egypt'),
                                                                        ('grace@example.com',   'Grace',   'Nakamura',  'Tokyo',         'Japan'),
                                                                        ('hamish@example.com',  'Hamish',  'MacDonald', 'Edinburgh',     'UK'),
                                                                        ('ines@example.com',    'Inés',    'Morales',   'Mexico City',   'Mexico'),
                                                                        ('james@example.com',   'James',   'Osei',      'Accra',         'Ghana');

-- ── Orders ───────────────────────────────────────────────────

CREATE TABLE orders (
                        id          SERIAL PRIMARY KEY,
                        customer_id INTEGER NOT NULL REFERENCES customers(id),
                        status      order_status NOT NULL DEFAULT 'pending',
                        total       NUMERIC(10,2) NOT NULL DEFAULT 0,
                        created_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO orders (customer_id, status, total) VALUES
                                                    (1,  'delivered',  38.97),
                                                    (1,  'shipped',    14.99),
                                                    (2,  'delivered',  22.98),
                                                    (3,  'delivered',  15.99),
                                                    (4,  'confirmed',  31.98),
                                                    (5,  'delivered',  28.98),
                                                    (6,  'pending',    16.99),
                                                    (7,  'delivered',  40.97),
                                                    (8,  'shipped',    13.99),
                                                    (9,  'delivered',  24.98),
                                                    (10, 'cancelled',  14.99),
                                                    (1,  'delivered',  19.99),
                                                    (2,  'pending',    15.99),
                                                    (3,  'confirmed',  26.98),
                                                    (7,  'delivered',  27.98);

-- ── Order Items ──────────────────────────────────────────────

CREATE TABLE order_items (
                             id        SERIAL PRIMARY KEY,
                             order_id  INTEGER NOT NULL REFERENCES orders(id),
                             book_id   INTEGER NOT NULL REFERENCES books(id),
                             quantity  INTEGER NOT NULL DEFAULT 1,
                             unit_price NUMERIC(8,2) NOT NULL
);

INSERT INTO order_items (order_id, book_id, quantity, unit_price) VALUES
                                                                      (1,  3,  1, 10.99),   -- Alice: 1984
                                                                      (1,  11, 1, 16.99),   -- Alice: Dune
                                                                      (1,  7,  1, 14.99),   -- Alice: Norwegian Wood  (total = 42.97, but we'll keep it illustrative)
                                                                      (2,  11, 1, 14.99),   -- Alice: Dune Messiah
                                                                      (3,  13, 1, 11.99),   -- Bob: Orient Express
                                                                      (3,  4,  1, 8.99),    -- Bob: Animal Farm (22.98 shown)
                                                                      (4,  15, 1, 15.99),   -- Clara: 100 Years
                                                                      (5,  3,  1, 10.99),   -- Dimitri: 1984
                                                                      (5,  16, 1, 13.99),   -- Dimitri: Handmaid's Tale
                                                                      (6,  5,  1, 14.99),   -- Elena: Beloved
                                                                      (6,  6,  1, 13.49),   -- Elena: Song of Solomon
                                                                      (7,  11, 1, 16.99),   -- Farouk: Dune
                                                                      (8,  1,  1, 12.99),   -- Grace: Left Hand of Darkness
                                                                      (8,  2,  1, 13.99),   -- Grace: Dispossessed
                                                                      (8,  19, 1, 13.99),   -- Grace: Never Let Me Go
                                                                      (9,  18, 1, 13.99),   -- Hamish: Never Let Me Go
                                                                      (10, 9,  1, 22.99),   -- Inés: Half of Yellow Sun
                                                                      (10, 4,  1, 8.99),    -- Inés: Animal Farm (total ~31.98 shown)
                                                                      (11, 12, 1, 14.99),   -- James: Dune Messiah (cancelled)
                                                                      (12, 19, 1, 19.99),   -- Alice: Remains of the Day
                                                                      (13, 15, 1, 15.99),   -- Bob: 100 Years
                                                                      (14, 3,  1, 10.99),   -- Clara: 1984
                                                                      (14, 17, 1, 14.99),   -- Clara: Oryx and Crake
                                                                      (15, 7,  1, 14.99),   -- Grace: Norwegian Wood
                                                                      (15, 8,  1, 15.99);   -- Grace: Kafka on the Shore

-- ── Reviews ──────────────────────────────────────────────────

CREATE TABLE reviews (
                         id          SERIAL PRIMARY KEY,
                         book_id     INTEGER NOT NULL REFERENCES books(id),
                         customer_id INTEGER NOT NULL REFERENCES customers(id),
                         rating      INTEGER NOT NULL,   -- 1-5
                         body        TEXT,
                         created_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO reviews (book_id, customer_id, rating, body) VALUES
                                                             (3,  1, 5, 'A terrifying vision that feels more relevant than ever.'),
                                                             (11, 1, 5, 'The world-building is unmatched. A desert planet that feels completely alive.'),
                                                             (7,  1, 4, 'Quietly devastating. Murakami at his most accessible.'),
                                                             (13, 2, 4, 'Christie plots like a clockwork machine. Brilliant misdirection.'),
                                                             (4,  2, 5, 'Short, sharp, and devastating. Every line earns its place.'),
                                                             (15, 3, 5, 'A century of life compressed into something that feels mythic.'),
                                                             (3,  4, 4, 'Orwell''s prose is ice-cold and precise. Haunting.'),
                                                             (16, 4, 5, 'Atwood''s world is oppressive in the best possible way.'),
                                                             (5,  5, 5, 'Morrison writes sentences that feel like they were carved from stone.'),
                                                             (6,  5, 4, 'Rich and sprawling. Morrison''s control of voice is extraordinary.'),
                                                             (11, 6, 5, 'Dune is one of those books that changes how you think about ecology.'),
                                                             (1,  7, 5, 'Le Guin dismantles gender so elegantly you barely notice it happening.'),
                                                             (2,  7, 5, 'The Dispossessed asks hard questions and refuses easy answers.'),
                                                             (18, 7, 4, 'Ishiguro''s restraint is heartbreaking. Stevens is one of literature''s great characters.'),
                                                             (18, 8, 5, 'Perfect. One of those books you read slowly to make it last.'),
                                                             (9,  9, 4, 'Adichie brings Nigeria''s civil war vividly to life.'),
                                                             (8,  7, 5, 'Kafka on the Shore defies explanation and that''s exactly the point.');

-- ============================================================
-- Sample Queries to Explore
-- ============================================================
-- Uncomment any query below to run it. Block comments make
-- it easy to select and copy a single query.

-- ── Basic Joins & Filters ──────────────────────────────────

/* All books by a given author:
SELECT b.title, b.price, b.genre
FROM books b JOIN authors a ON b.author_id = a.id
WHERE a.last_name = 'Ishiguro';
*/

/* Orders with their items:
SELECT o.id, c.email, b.title, oi.quantity, oi.unit_price
FROM orders o
JOIN customers c ON c.id = o.customer_id
JOIN order_items oi ON oi.order_id = o.id
JOIN books b ON b.id = oi.book_id
ORDER BY o.id;
*/

/* Books low on stock:
SELECT title, stock FROM books WHERE stock < 10 ORDER BY stock;
*/

/* Books published between two dates:
SELECT title, published, price
FROM books
WHERE published BETWEEN '1960-01-01' AND '1989-12-31'
ORDER BY published;
*/

-- ── Aggregation & HAVING ───────────────────────────────────

/* Top-rated books (average rating >= 4):
SELECT b.title, ROUND(AVG(r.rating), 2) AS avg_rating, COUNT(*) AS review_count
FROM books b JOIN reviews r ON r.book_id = b.id
GROUP BY b.title
HAVING AVG(r.rating) >= 4
ORDER BY avg_rating DESC;
*/

/* Best customers by spend:
SELECT c.first_name, c.last_name, SUM(o.total) AS total_spend
FROM customers c JOIN orders o ON o.customer_id = c.id
WHERE o.status != 'cancelled'
GROUP BY c.first_name, c.last_name
ORDER BY total_spend DESC;
*/

/* Revenue by genre:
SELECT b.genre, SUM(oi.quantity * oi.unit_price) AS revenue, SUM(oi.quantity) AS units_sold
FROM order_items oi
JOIN books b ON b.id = oi.book_id
JOIN orders o ON o.id = oi.order_id
WHERE o.status != 'cancelled'
GROUP BY b.genre
ORDER BY revenue DESC;
*/

/* Authors ranked by number of reviews:
SELECT a.first_name, a.last_name, COUNT(r.id) AS total_reviews, ROUND(AVG(r.rating), 2) AS avg_rating
FROM authors a
JOIN books b ON b.author_id = a.id
JOIN reviews r ON r.book_id = b.id
GROUP BY a.first_name, a.last_name
ORDER BY total_reviews DESC;
*/

-- ── CASE WHEN ──────────────────────────────────────────────

/* Classify books by price tier:
SELECT title, price,
  CASE
    WHEN price < 12 THEN 'budget'
    WHEN price < 17 THEN 'mid-range'
    ELSE 'premium'
  END AS price_tier
FROM books
ORDER BY price;
*/

/* Order status summary:
SELECT
  COUNT(*) AS total_orders,
  COUNT(*) FILTER (WHERE status = 'delivered') AS delivered,
  COUNT(*) FILTER (WHERE status = 'shipped') AS shipped,
  COUNT(*) FILTER (WHERE status = 'pending') AS pending,
  COUNT(*) FILTER (WHERE status = 'cancelled') AS cancelled
FROM orders;
*/

-- ── Subqueries & EXISTS ────────────────────────────────────

/* Books that have never been ordered:
SELECT title, price
FROM books b
WHERE NOT EXISTS (
  SELECT 1 FROM order_items oi WHERE oi.book_id = b.id
);
*/

/* Customers who have ordered books by more than one author:
SELECT c.first_name, c.last_name
FROM customers c
WHERE (
  SELECT COUNT(DISTINCT b.author_id)
  FROM orders o
  JOIN order_items oi ON oi.order_id = o.id
  JOIN books b ON b.id = oi.book_id
  WHERE o.customer_id = c.id
) > 1;
*/

/* Books priced above the average:
SELECT title, price
FROM books
WHERE price > (SELECT AVG(price) FROM books)
ORDER BY price DESC;
*/

-- ── IN & NOT IN ────────────────────────────────────────────

/* Authors whose books have all been reviewed:
SELECT a.first_name, a.last_name
FROM authors a
WHERE a.id NOT IN (
  SELECT b.author_id FROM books b
  WHERE b.id NOT IN (SELECT r.book_id FROM reviews r)
);
*/

/* Genres that appear in cancelled orders:
SELECT DISTINCT b.genre
FROM books b
WHERE b.id IN (
  SELECT oi.book_id FROM order_items oi
  JOIN orders o ON o.id = oi.order_id
  WHERE o.status = 'cancelled'
);
*/

-- ── COALESCE & NULLIF ──────────────────────────────────────

/* Books with description (or a fallback):
SELECT title, COALESCE(description, '(no description)') AS description
FROM books;
*/

/* Average rating, treating unreviewed books as NULL:
SELECT b.title,
  COALESCE(ROUND(AVG(r.rating), 2), 0) AS avg_rating,
  COALESCE(COUNT(r.id), 0) AS review_count
FROM books b
LEFT JOIN reviews r ON r.book_id = b.id
GROUP BY b.title
ORDER BY avg_rating DESC;
*/

-- ── STRING_AGG & ARRAY_AGG ─────────────────────────────────

/* Each author with their book titles concatenated:
SELECT a.first_name || ' ' || a.last_name AS author,
  STRING_AGG(b.title, ', ' ORDER BY b.published) AS books
FROM authors a
JOIN books b ON b.author_id = a.id
GROUP BY a.first_name, a.last_name;
*/

/* Each customer's ordered book titles as an array:
SELECT c.first_name, ARRAY_AGG(DISTINCT b.title) AS books_ordered
FROM customers c
JOIN orders o ON o.customer_id = c.id
JOIN order_items oi ON oi.order_id = o.id
JOIN books b ON b.id = oi.book_id
GROUP BY c.first_name;
*/

-- ── Set Operations ─────────────────────────────────────────

/* Authors who have been both reviewed AND ordered (INTERSECT):
SELECT DISTINCT a.first_name, a.last_name
FROM authors a JOIN books b ON b.author_id = a.id JOIN reviews r ON r.book_id = b.id
INTERSECT
SELECT DISTINCT a.first_name, a.last_name
FROM authors a JOIN books b ON b.author_id = a.id JOIN order_items oi ON oi.book_id = b.id;
*/

/* Authors reviewed but never ordered (EXCEPT):
SELECT DISTINCT a.first_name, a.last_name
FROM authors a JOIN books b ON b.author_id = a.id JOIN reviews r ON r.book_id = b.id
EXCEPT
SELECT DISTINCT a.first_name, a.last_name
FROM authors a JOIN books b ON b.author_id = a.id JOIN order_items oi ON oi.book_id = b.id;
*/

-- ── Correlated Subqueries ──────────────────────────────────

/* Each author's most expensive book:
SELECT a.first_name, a.last_name, b.title, b.price
FROM books b
JOIN authors a ON a.id = b.author_id
WHERE b.price = (
  SELECT MAX(b2.price) FROM books b2 WHERE b2.author_id = b.author_id
);
*/

/* Customers whose total spend exceeds the average customer spend:
SELECT c.first_name, c.last_name, SUM(o.total) AS total_spend
FROM customers c
JOIN orders o ON o.customer_id = c.id
WHERE o.status != 'cancelled'
GROUP BY c.first_name, c.last_name
HAVING SUM(o.total) > (
  SELECT AVG(customer_total) FROM (
    SELECT SUM(o2.total) AS customer_total
    FROM orders o2
    WHERE o2.status != 'cancelled'
    GROUP BY o2.customer_id
  )
);
*/
