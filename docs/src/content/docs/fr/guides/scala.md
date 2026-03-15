---
title: Utilisation Scala
description: Utiliser PetraDB depuis Scala sur JVM, JS et Native.
---

## Base de données en mémoire

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

Chaque instance `MemoryDB` est isolée et indépendante. Toutes les données restent en mémoire.

## Base de données persistante

PetraDB supporte le stockage durable résistant aux pannes sur JVM et Native via [stow](https://github.com/edadma/stow).

### Créer une nouvelle base de données

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

### Rouvrir une base de données existante

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

Toutes les tables, données, types enum et l'état d'auto-incrément sont restaurés à la réouverture.

Les bases de données persistantes utilisent des pages en copie-sur-écriture et des en-têtes à double tampon pour la résistance aux pannes. Toutes les opérations DDL et DML sont durables.

## Base de données texte

`TextDB` stocke la base de données sous forme de fichier `.ptxt` éditable par l'homme. Elle charge en mémoire à l'ouverture et réécrit le fichier après chaque modification. Idéale pour le développement initial, la configuration et le contrôle de version.

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

Rouvrez le même fichier pour restaurer toutes les données :

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

Fonctionne sur JVM et Native. Le format `.ptxt` est lisible par l'homme et compatible avec les diffs. Utilisez `PersistentDB` lorsque vous avez besoin d'une durabilité résistante aux pannes pour les données de production.

## Exécuter du SQL

`executeSQL(sql)` exécute une ou plusieurs instructions séparées par des points-virgules et retourne un `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

Consultez la [référence API Scala](/reference/api-scala/) pour les types de résultats, l'extraction de valeurs et l'API complète.

## Tests

```bash
# Exécuter les tests pour toutes les plateformes
sbt test

# Exécuter les tests JavaScript uniquement
sbt engineJS/test

# Exécuter les tests JVM uniquement
sbt engineJVM/test

# Exécuter les tests Native uniquement
sbt engineNative/test
```

## Notes sur les plateformes

- **JVM** — thread-safe, s'intègre avec Spring Boot, Play Framework, Akka, etc.
- **Scala.js** — fonctionne dans Node.js et les navigateurs
- **Scala Native** — compile en exécutables natifs ; idéal pour les outils CLI et les systèmes embarqués
