---
title: Utilisation Scala
description: Utiliser PetraDB depuis Scala sur JVM, JS et Native.
---

## Base de donnees en memoire

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE products (
    id SERIAL,
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

Chaque instance `MemoryDB` est isolee et independante. Toutes les donnees restent en memoire.

## Base de donnees persistante

PetraDB supporte le stockage durable resistant aux pannes sur JVM et Native via [stow](https://github.com/edadma/stow).

### Creer une nouvelle base de donnees

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = PersistentDB.create("mydata.db", 4096)
given Session = db.connect()

executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  );

  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
""")

db.close()
```

### Rouvrir une base de donnees existante

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

Toutes les tables, donnees, types enum et l'etat d'auto-increment sont restaures a la reouverture.

Les bases de donnees persistantes utilisent des pages en copie-sur-ecriture et des en-tetes a double tampon pour la resistance aux pannes. Toutes les operations DDL et DML sont durables.

## Base de donnees texte

`TextDB` stocke la base de donnees sous forme de fichier `.ptxt` editable par l'homme. Elle charge en memoire a l'ouverture et reecrit le fichier apres chaque modification. Ideale pour le developpement initial, la configuration et le controle de version.

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

executeSQL("""
  CREATE TABLE settings (key TEXT, value TEXT);
  INSERT INTO settings (key, value) VALUES ('theme', 'dark');
""")

db.close()
```

Rouvrez le meme fichier pour restaurer toutes les donnees :

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

Fonctionne sur JVM et Native. Le format `.ptxt` est lisible par l'homme et compatible avec les diffs. Utilisez `PersistentDB` lorsque vous avez besoin d'une durabilite resistante aux pannes pour les donnees de production.

## Executer du SQL

`executeSQL(sql)` execute une ou plusieurs instructions separees par des points-virgules et retourne un `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

Consultez la [reference API Scala](/reference/api-scala/) pour les types de resultats, l'extraction de valeurs et l'API complete.

## Tests

```bash
# Executer les tests pour toutes les plateformes
sbt test

# Executer les tests JavaScript uniquement
sbt engineJS/test

# Executer les tests JVM uniquement
sbt engineJVM/test

# Executer les tests Native uniquement
sbt engineNative/test
```

## Notes sur les plateformes

- **JVM** -- thread-safe, s'integre avec Spring Boot, Play Framework, Akka, etc.
- **Scala.js** -- fonctionne dans Node.js et les navigateurs
- **Scala Native** -- compile en executables natifs ; ideal pour les outils CLI et les systemes embarques
