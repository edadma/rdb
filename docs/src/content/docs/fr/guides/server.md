---
title: Serveur
description: Executer PetraDB en tant que serveur HTTP.
---

## Installation

```bash
npm install -g @petradb/server
```

## Utilisation

```bash
petradb-server [OPTIONS] [path]
```

Si un chemin est donne, le serveur ouvre (ou cree) une base de donnees persistante a cet emplacement. Sans chemin, une base de donnees en memoire est utilisee.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Utiliser une base de donnees en memoire |
| `-p`, `--port` | Numero de port (par defaut : `5480`) |
| `-h`, `--host` | Adresse de l'hote (par defaut : `127.0.0.1`) |
| `-c`, `--config` | Chemin vers un fichier de configuration TOML |

### Exemples

```bash
# Base de donnees en memoire sur le port par defaut
petradb-server

# Base de donnees persistante sur un port personnalise
petradb-server -p 8080 mydata.db

# Avec un fichier de configuration
petradb-server -c petradb.toml mydata.db
```

## Configuration

Un fichier TOML controle l'authentification, le CORS et les limites de sessions.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (par defaut), "none", ou une origine specifique

[sessions]
max_sessions = 100    # 0 = illimite (par defaut)
```

### Modes d'authentification

| Mode | Description |
|------|-------------|
| `"none"` | Pas d'authentification (par defaut sans fichier de configuration) |
| `"basic"` | Authentification HTTP Basic contre la liste `[[users]]` |

Les mots de passe dans le fichier de configuration sont stockes sous forme de hachages PBKDF2.

Lorsque `auth = "basic"`, chaque requete (sauf `GET /health`) doit inclure un en-tete `Authorization: Basic <credentials>`.

### CORS

| Valeur de `origin` | Comportement |
|---------------------|-------------|
| `"*"` (par defaut) | Autoriser toutes les origines |
| `"none"` | Pas d'en-tetes CORS |
| Une URL (ex. `"https://app.example.com"`) | Autoriser uniquement cette origine |

## API HTTP

Toutes les requetes et reponses SQL utilisent le codec binaire PetraDB (`application/octet-stream`). Utilisez la bibliotheque [`@petradb/client`](/guides/client/) au lieu d'appeler ces endpoints directement.

### Executer du SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (optionnel)

<sql text>
```

Retourne un `Seq[Result]` encode en binaire. Sans en-tete `X-Session-Id`, chaque requete s'execute dans une session transitoire unique.

### Creer une session

```
POST /session
```

Retourne `{ "sessionId": "<id>" }`. Utilisez l'ID retourne dans les en-tetes `X-Session-Id` subsequents pour partager l'etat transactionnel entre les requetes.

### Fermer une session

```
DELETE /session/<id>
```

Retourne `{ "ok": "true" }` en cas de succes, ou `404` si la session n'existe pas.

### Verification de sante

```
GET /health
```

Retourne `{ "status": "ok" }`. Non soumis a l'authentification.

## Reponses d'erreur

| Statut | Signification |
|--------|--------------|
| 400 | Erreur de syntaxe SQL, erreur de type ou reference indefinie |
| 401 | Identifiants manquants ou invalides |
| 409 | Violation de schema ou de contrainte |
| 503 | Limite de sessions atteinte |
