---
title: Servidor
description: Ejecutar PetraDB como un servidor HTTP.
---

## Instalacion

```bash
npm install -g @petradb/server
```

## Uso

```bash
petradb-server [OPCIONES] [ruta]
```

Si se proporciona una ruta, el servidor abre (o crea) una base de datos persistente en esa ubicacion. Sin una ruta, se usa una base de datos en memoria.

### Opciones

| Opcion | Descripcion |
|--------|-------------|
| `-m`, `--memory` | Usar una base de datos en memoria |
| `-p`, `--port` | Numero de puerto (por defecto: `5480`) |
| `-h`, `--host` | Direccion del host (por defecto: `127.0.0.1`) |
| `-c`, `--config` | Ruta a un archivo de configuracion TOML |

### Ejemplos

```bash
# Base de datos en memoria en el puerto por defecto
petradb-server

# Base de datos persistente en un puerto personalizado
petradb-server -p 8080 mydata.db

# Con un archivo de configuracion
petradb-server -c petradb.toml mydata.db
```

## Configuracion

Un archivo TOML controla la autenticacion, CORS y limites de sesion.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (por defecto), "none", o un origen especifico

[sessions]
max_sessions = 100    # 0 = ilimitado (por defecto)
```

### Modos de autenticacion

| Modo | Descripcion |
|------|-------------|
| `"none"` | Sin autenticacion (por defecto cuando no se proporciona archivo de configuracion) |
| `"basic"` | Autenticacion HTTP Basic contra la lista `[[users]]` |

Las contrasenas en el archivo de configuracion se almacenan como hashes PBKDF2.

Cuando `auth = "basic"`, cada solicitud (excepto `GET /health`) debe incluir un encabezado `Authorization: Basic <credenciales>`.

### CORS

| Valor de `origin` | Comportamiento |
|----------------|-----------|
| `"*"` (por defecto) | Permitir todos los origenes |
| `"none"` | Sin encabezados CORS |
| Una URL (ej. `"https://app.example.com"`) | Permitir solo ese origen |

## API HTTP

Todas las solicitudes y respuestas SQL usan el codec binario de PetraDB (`application/octet-stream`). Usa la biblioteca [`@petradb/client`](/guides/client/) en lugar de llamar a estos endpoints directamente.

### Ejecutar SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (opcional)

<texto sql>
```

Retorna un `Seq[Result]` codificado en binario. Sin un encabezado `X-Session-Id`, cada solicitud se ejecuta en una sesion transitoria de un solo uso.

### Crear sesion

```
POST /session
```

Retorna `{ "sessionId": "<id>" }`. Usa el ID retornado en los encabezados `X-Session-Id` subsecuentes para compartir estado de transaccion entre solicitudes.

### Cerrar sesion

```
DELETE /session/<id>
```

Retorna `{ "ok": "true" }` en caso de exito, o `404` si la sesion no existe.

### Verificacion de salud

```
GET /health
```

Retorna `{ "status": "ok" }`. No esta sujeto a autenticacion.

## Respuestas de error

| Estado | Significado |
|--------|---------|
| 400 | Error de analisis SQL, error de tipo o referencia indefinida |
| 401 | Credenciales faltantes o invalidas |
| 409 | Violacion de esquema o restriccion |
| 503 | Limite de sesiones alcanzado |
