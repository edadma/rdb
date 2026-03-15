---
title: Serveur
description: Exécuter PetraDB en tant que serveur HTTP.
---

## Installation

```bash
npm install -g @petradb/server
```

## Utilisation

```bash
petradb-server [OPTIONS] [path]
```

Si un chemin est donné, le serveur ouvre (ou crée) une base de données persistante à cet emplacement. Sans chemin, une base de données en mémoire est utilisée.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Utiliser une base de données en mémoire |
| `-p`, `--port` | Numéro de port (par défaut : `5480`) |
| `-h`, `--host` | Adresse de l'hôte (par défaut : `127.0.0.1`) |
| `-c`, `--config` | Chemin vers un fichier de configuration TOML |

### Exemples

```bash
# Base de données en mémoire sur le port par défaut
petradb-server

# Base de données persistante sur un port personnalisé
petradb-server -p 8080 mydata.db

# Avec un fichier de configuration
petradb-server -c petradb.toml mydata.db
```

## Configuration

Un fichier TOML contrôle l'authentification, le CORS et les limites de sessions.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (par défaut), "none", ou une origine spécifique

[sessions]
max_sessions = 100    # 0 = illimité (par défaut)
```

### Modes d'authentification

| Mode | Description |
|------|-------------|
| `"none"` | Pas d'authentification (par défaut sans fichier de configuration) |
| `"basic"` | Authentification HTTP Basic contre la liste `[[users]]` |

Les mots de passe dans le fichier de configuration sont stockés sous forme de hachages PBKDF2.

Lorsque `auth = "basic"`, chaque requête (sauf `GET /health`) doit inclure un en-tête `Authorization: Basic <credentials>`.

### CORS

| Valeur de `origin` | Comportement |
|---------------------|-------------|
| `"*"` (par défaut) | Autoriser toutes les origines |
| `"none"` | Pas d'en-têtes CORS |
| Une URL (ex. `"https://app.example.com"`) | Autoriser uniquement cette origine |

## API HTTP

Toutes les requêtes et réponses SQL utilisent le codec binaire PetraDB (`application/octet-stream`). Utilisez la bibliothèque [`@petradb/client`](/guides/client/) au lieu d'appeler ces endpoints directement.

### Exécuter du SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (optionnel)

<sql text>
```

Retourne un `Seq[Result]` encodé en binaire. Sans en-tête `X-Session-Id`, chaque requête s'exécute dans une session transitoire unique.

### Créer une session

```
POST /session
```

Retourne `{ "sessionId": "<id>" }`. Utilisez l'ID retourné dans les en-têtes `X-Session-Id` subséquents pour partager l'état transactionnel entre les requêtes.

### Fermer une session

```
DELETE /session/<id>
```

Retourne `{ "ok": "true" }` en cas de succès, ou `404` si la session n'existe pas.

### Vérification de santé

```
GET /health
```

Retourne `{ "status": "ok" }`. Non soumis à l'authentification.

## Réponses d'erreur

| Statut | Signification |
|--------|--------------|
| 400 | Erreur de syntaxe SQL, erreur de type ou référence indéfinie |
| 401 | Identifiants manquants ou invalides |
| 409 | Violation de schéma ou de contrainte |
| 503 | Limite de sessions atteinte |
