---
title: Primeros pasos con Java
description: Agrega PetraDB a tu proyecto Java via JDBC y ejecuta tus primeras consultas SQL.
---

PetraDB proporciona un driver JDBC 4 estandar, por lo que usas la API familiar `java.sql` — `Connection`, `Statement`, `ResultSet`, `PreparedStatement`.

## Instalacion

**Maven:**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.0</version>
</dependency>
```

**Gradle:**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.0'
```

El driver se registra automaticamente — no se necesita `Class.forName()`.

## Ejecuta tu primera consulta

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

Salida:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Almacenamiento persistente

Para datos que sobrevivan a los reinicios, usa una URL de conexion de archivo:

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

El archivo se crea en el primer uso. Extensiones soportadas:

| Extension | Tipo de almacenamiento |
|-----------|-------------|
| `.petra` | Almacenamiento persistente a prueba de fallos |
| `.ptxt` | Formato de texto legible por humanos |

## Sentencias preparadas

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## Transacciones

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

## Siguientes pasos

Consulta la [referencia JDBC](/integrations/jdbc/) para la API completa, incluyendo operaciones por lotes, metadatos, mapeo de tipos y conexiones de servidor.
