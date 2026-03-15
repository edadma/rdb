---
title: PL/pgSQL
description: Lenguaje procedural — bloques DO, funciones almacenadas y procedimientos almacenados.
---

PetraDB soporta PL/pgSQL, el lenguaje procedural de PostgreSQL, para escribir logica de flujo de control, bucles y ejecucion SQL condicional.

## Bloques DO

Bloques anonimos que se ejecutan inmediatamente sin ser almacenados:

```sql
DO $$
DECLARE
  i INT;
  total INT := 0;
BEGIN
  FOR i IN 1..10 LOOP
    total := total + i;
  END LOOP;
  INSERT INTO results (sum) VALUES (total);
END $$;
```

## Funciones almacenadas

Las funciones retornan un valor y pueden ser llamadas en cualquier expresion SQL:

```sql
CREATE FUNCTION factorial(n INT) RETURNS INT AS $$
DECLARE
  result INT := 1;
  i INT := 1;
BEGIN
  WHILE i <= n LOOP
    result := result * i;
    i := i + 1;
  END LOOP;
  RETURN result;
END $$ LANGUAGE plpgsql;

SELECT factorial(5);  -- 120
```

Usa `CREATE OR REPLACE FUNCTION` para sobreescribir una funcion existente.

### Uso de funciones en consultas

Las funciones funcionan en cualquier lugar donde se permitan expresiones:

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## Procedimientos almacenados

Los procedimientos realizan acciones y se invocan con `CALL`:

```sql
CREATE PROCEDURE seed_users(n INT) AS $$
DECLARE
  i INT;
BEGIN
  FOR i IN 1..n LOOP
    INSERT INTO users (name) VALUES ('User ' || i::TEXT);
  END LOOP;
END $$ LANGUAGE plpgsql;

CALL seed_users(100);
```

Usa `CREATE OR REPLACE PROCEDURE` para sobreescribir un procedimiento existente.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## Estructura de bloque

Todos los bloques PL/pgSQL (bloques DO, cuerpos de funcion, cuerpos de procedimiento) comparten la misma estructura:

```
[DECLARE
  variable_name type [:= default_value];
  ...]
BEGIN
  statements...
[EXCEPTION
  WHEN condition THEN
    statements...]
END
```

## Variables

Declara variables con un tipo y un valor por defecto opcional:

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

Las variables sin valor por defecto se inicializan a `NULL`. La asignacion usa `:=`:

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

Las variables pueden usarse en cualquier sentencia SQL dentro del bloque — en `VALUES`, `WHERE`, `SET`, etc.

## Flujo de control

### IF / ELSIF / ELSE

```sql
IF amount > 1000 THEN
  INSERT INTO vip_orders VALUES (order_id);
ELSIF amount > 100 THEN
  INSERT INTO normal_orders VALUES (order_id);
ELSE
  INSERT INTO small_orders VALUES (order_id);
END IF;
```

### Bucle WHILE

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

Maximo 10,000 iteraciones como limite de seguridad.

### Bucle FOR de rango

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

La variable del bucle debe declararse en `DECLARE`. Ambos limites son inclusivos.

### Bucle FOR de consulta

Iterar sobre resultados de consulta:

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

Para consultas de una sola columna, la variable recibe el valor escalar. Para consultas de multiples columnas, la variable recibe un registro.

## RETURN

Salir de un bloque o retornar un valor desde una funcion:

```sql
-- En una funcion:
RETURN x * 2;

-- En un bloque DO o procedimiento (salir anticipadamente):
RETURN;
```

## RAISE

Imprimir mensajes o lanzar errores:

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

Los marcadores `%` se reemplazan por los valores de los argumentos en orden. `RAISE EXCEPTION` aborta la ejecucion.

## PERFORM

Ejecutar una consulta y descartar el resultado:

```sql
PERFORM SELECT notify_user(user_id);
```

## Manejo de excepciones

Capturar errores dentro de un bloque:

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- la tabla ya existe, ignorar
END $$;
```

Condiciones de excepcion:
- `duplicate_object` — tabla/tipo/restriccion ya existe
- `unique_violation` — restriccion de unicidad violada
- `others` — captura cualquier excepcion

## Sentencia NULL

Una sentencia sin operacion, comumente usada en manejadores de excepcion:

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## Triggers

Los triggers ejecutan una funcion automaticamente cuando se insertan, actualizan o eliminan filas:

```sql
CREATE FUNCTION audit_changes() RETURNS INT AS $$
BEGIN
  INSERT INTO audit_log VALUES (tg_op || ' on ' || tg_table_name);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();

CREATE TRIGGER trg_audit_del AFTER DELETE ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();
```

### Sintaxis

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### Momento de ejecucion

- Los triggers **BEFORE** se ejecutan antes de la operacion. Retorna `NULL` para cancelar la operacion de fila. Retorna cualquier valor no NULL para proceder.
- Los triggers **AFTER** se ejecutan despues de la operacion. El valor de retorno se ignora.

### Variables especiales

Las funciones de trigger tienen acceso a:

| Variable | Descripcion |
|----------|-------------|
| `tg_op` | Nombre de la operacion: `'INSERT'`, `'UPDATE'` o `'DELETE'` |
| `tg_table_name` | Nombre de la tabla que disparo el trigger |
| `OLD` | Fila antes de la operacion (UPDATE, DELETE) |
| `NEW` | Fila despues de la operacion (INSERT, UPDATE) |

### Eventos

Los triggers se disparan para:
- `INSERT` — incluyendo filas insertadas via `COPY FROM`
- `UPDATE` — se dispara por fila actualizada
- `DELETE` — se dispara por fila eliminada

### Ejemplo de trigger protector

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- cancelar el DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### Callbacks de funciones nativas

Las funciones nativas registradas (Scala, JavaScript o C) son invocables desde funciones de trigger, permitiendo integracion con sistemas externos:

```sql
-- Asumiendo que notify_webhook() esta registrada como funcion nativa
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

Consulta [API de Scala](/reference/api-scala/), [API de JavaScript](/reference/api-javascript/) y [API de C](/reference/api-c/) para registrar funciones nativas.

## Persistencia

Las funciones almacenadas, procedimientos y triggers persisten entre reinicios de la base de datos tanto para almacenamiento en memoria (duracion de la sesion) como persistente (sobrevive al cierre/reapertura). El SQL fuente se almacena y se re-ejecuta al abrir la base de datos.

## Composicion

Las funciones pueden llamar a otras funciones. Los procedimientos pueden llamar a funciones:

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```
