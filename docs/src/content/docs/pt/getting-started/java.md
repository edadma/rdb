---
title: Primeiros Passos com Java
description: Adicione o PetraDB ao seu projeto Java via JDBC e execute suas primeiras consultas SQL.
---

O PetraDB fornece um driver JDBC 4 padrao, entao voce usa a API familiar `java.sql` — `Connection`, `Statement`, `ResultSet`, `PreparedStatement`.

## Instalacao

**Maven:**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.2</version>
</dependency>
```

**Gradle:**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.2'
```

O driver se registra automaticamente — nao e necessario `Class.forName()`.

## Execute sua primeira consulta

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

Saida:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Armazenamento persistente

Para dados que sobrevivam a reinicializacoes, use uma URL de conexao de arquivo:

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

O arquivo e criado no primeiro uso. Extensoes suportadas:

| Extensao | Tipo de armazenamento |
|-----------|-------------|
| `.petra` | Armazenamento persistente seguro contra falhas |
| `.ptxt` | Formato de texto legivel por humanos |

## Prepared statements

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## Transacoes

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

## Proximos passos

Veja a [referencia JDBC](/integrations/jdbc/) para a API completa, incluindo operacoes em lote, metadados, mapeamento de tipos e conexoes com servidor.
