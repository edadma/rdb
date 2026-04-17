---
title: Premiers pas avec Java
description: Ajoutez PetraDB à votre projet Java via JDBC et exécutez vos premières requêtes SQL.
---

PetraDB fournit un pilote JDBC 4 standard, vous utilisez donc l'API familière `java.sql` — `Connection`, `Statement`, `ResultSet`, `PreparedStatement`.

## Installation

**Maven :**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.2</version>
</dependency>
```

**Gradle :**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.2'
```

Le pilote s'enregistre automatiquement — pas besoin de `Class.forName()`.

## Exécutez votre première requête

```java
import java.sql.*;

public class Main {
    public static void main(String[] args) throws SQLException {
        Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
        Statement stmt = conn.createStatement();

        stmt.executeUpdate("""
            CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                name TEXT NOT NULL,
                email TEXT
            )
        """);

        stmt.executeUpdate("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
        stmt.executeUpdate("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

        ResultSet rs = stmt.executeQuery("SELECT id, name, email FROM users ORDER BY id");
        while (rs.next()) {
            System.out.printf("%d: %s <%s>%n",
                rs.getInt("id"),
                rs.getString("name"),
                rs.getString("email"));
        }

        rs.close();
        stmt.close();
        conn.close();
    }
}
```

Sortie :
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Stockage persistant

Pour des données qui survivent aux redémarrages, utilisez une URL de connexion fichier :

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

Le fichier est créé lors de la première utilisation. Extensions supportées :

| Extension | Type de stockage |
|-----------|-----------------|
| `.petra` | Stockage persistant résistant aux pannes |
| `.ptxt` | Format texte lisible par l'homme |

## Prepared statements

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## Transactions

```java
conn.setAutoCommit(false);
try {
    stmt.executeUpdate("UPDATE accounts SET balance = balance - 100 WHERE id = 1");
    stmt.executeUpdate("UPDATE accounts SET balance = balance + 100 WHERE id = 2");
    conn.commit();
} catch (SQLException e) {
    conn.rollback();
}
```

## Étapes suivantes

Consultez la [référence JDBC](/integrations/jdbc/) pour l'API complète, y compris les opérations par lots, les métadonnées, le mapping de types et les connexions serveur.
