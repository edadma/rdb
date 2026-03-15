---
title: CLI
description: Shell SQL interactif et outils en ligne de commande pour PetraDB.
---

## Installation

```bash
npm install -g @petradb/cli
```

## Utilisation

```bash
petradb [OPTIONS] [path]
```

Si un chemin est donné, le CLI ouvre (ou crée) une base de données persistante à cet emplacement. Sans chemin, une base de données en mémoire est utilisée.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Utiliser une base de données en mémoire (même si un chemin est donné) |
| `-e`, `--execute` | Exécuter du SQL et quitter (répétable) |
| `-f`, `--file` | Exécuter un fichier SQL et quitter (répétable) |
| `--stdin` | Lire le SQL depuis stdin et quitter |
| `--host` | Se connecter à un serveur PetraDB distant |
| `--port` | Port du serveur (par défaut : 5480) |
| `--user` | Nom d'utilisateur pour l'authentification |
| `--password` | Mot de passe pour l'authentification |

### Exemples

```bash
# Démarrer une session interactive en mémoire
petradb

# Ouvrir ou créer une base de données persistante
petradb mydata.db

# Exécuter du SQL de manière non interactive
petradb mydata.db -e "SELECT * FROM users"

# Exécuter un fichier puis une requête
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# Passer du SQL depuis stdin
echo "SELECT 1 + 1" | petradb --stdin

# Se connecter à un serveur distant
petradb --host myserver.example.com --port 5480

# Se connecter avec authentification
petradb --host localhost --user admin --password secret
```

## Modes de base de données

L'extension du fichier détermine le mode de stockage :

| Extension | Mode | Description |
|-----------|------|-------------|
| `.petra` (ou toute autre) | Persistant | Stockage durable résistant aux pannes via pages en copie-sur-écriture |
| `.ptxt` | Texte | Fichier lisible par l'homme, réécrit après chaque modification |
| *(pas de chemin)* | En mémoire | Les données existent uniquement pour la durée de la session |

## Dump

La sous-commande `dump` affiche le schéma complet et les données d'une base de données persistante sous forme d'instructions SQL.

```bash
petradb dump mydata.db
```

La sortie inclut les instructions `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO` et `CREATE VIEW` permettant de recréer la base de données à partir de zéro.

## REPL interactif

Lorsqu'il est démarré sans `-e`, `-f` ou `--stdin`, le CLI entre dans un shell SQL interactif avec historique readline.

Tapez du SQL terminé par `;` pour l'exécuter. La saisie multi-lignes est supportée — une invite de continuation (`  -> `) apparaît jusqu'à ce que vous terminiez par `;`.

### Méta-commandes

| Commande | Description |
|----------|-------------|
| `\dt` | Lister toutes les tables |
| `\dv` | Lister toutes les vues |
| `\ds` | Lister toutes les séquences |
| `\di` | Lister tous les index |
| `\d <name>` | Décrire une table ou une vue (colonnes, types, nullabilité) |
| `\i <file>` | Exécuter du SQL depuis un fichier |
| `\dump` | Afficher le schéma complet et les données en SQL |
| `\copy <args>` | Exécuter une instruction `COPY` |
| `\timing` | Activer/désactiver le chronométrage des requêtes |
| `\q` | Quitter |

### Exemple de session

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```
