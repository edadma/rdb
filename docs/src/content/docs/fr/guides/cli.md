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

Si un chemin est donne, le CLI ouvre (ou cree) une base de donnees persistante a cet emplacement. Sans chemin, une base de donnees en memoire est utilisee.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Utiliser une base de donnees en memoire (meme si un chemin est donne) |
| `-e`, `--execute` | Executer du SQL et quitter (repetable) |
| `-f`, `--file` | Executer un fichier SQL et quitter (repetable) |
| `--stdin` | Lire le SQL depuis stdin et quitter |
| `--host` | Se connecter a un serveur PetraDB distant |
| `--port` | Port du serveur (par defaut : 5480) |
| `--user` | Nom d'utilisateur pour l'authentification |
| `--password` | Mot de passe pour l'authentification |

### Exemples

```bash
# Demarrer une session interactive en memoire
petradb

# Ouvrir ou creer une base de donnees persistante
petradb mydata.db

# Executer du SQL de maniere non interactive
petradb mydata.db -e "SELECT * FROM users"

# Executer un fichier puis une requete
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# Passer du SQL depuis stdin
echo "SELECT 1 + 1" | petradb --stdin

# Se connecter a un serveur distant
petradb --host myserver.example.com --port 5480

# Se connecter avec authentification
petradb --host localhost --user admin --password secret
```

## Modes de base de donnees

L'extension du fichier determine le mode de stockage :

| Extension | Mode | Description |
|-----------|------|-------------|
| `.petra` (ou toute autre) | Persistant | Stockage durable resistant aux pannes via pages en copie-sur-ecriture |
| `.ptxt` | Texte | Fichier lisible par l'homme, reecrit apres chaque modification |
| *(pas de chemin)* | En memoire | Les donnees existent uniquement pour la duree de la session |

## Dump

La sous-commande `dump` affiche le schema complet et les donnees d'une base de donnees persistante sous forme d'instructions SQL.

```bash
petradb dump mydata.db
```

La sortie inclut les instructions `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO` et `CREATE VIEW` permettant de recreer la base de donnees a partir de zero.

## REPL interactif

Lorsqu'il est demarre sans `-e`, `-f` ou `--stdin`, le CLI entre dans un shell SQL interactif avec historique readline.

Tapez du SQL termine par `;` pour l'executer. La saisie multi-lignes est supportee -- une invite de continuation (`  -> `) apparait jusqu'a ce que vous terminiez par `;`.

### Meta-commandes

| Commande | Description |
|----------|-------------|
| `\dt` | Lister toutes les tables |
| `\dv` | Lister toutes les vues |
| `\ds` | Lister toutes les sequences |
| `\di` | Lister tous les index |
| `\d <name>` | Decrire une table ou une vue (colonnes, types, nullabilite) |
| `\i <file>` | Executer du SQL depuis un fichier |
| `\dump` | Afficher le schema complet et les donnees en SQL |
| `\copy <args>` | Executer une instruction `COPY` |
| `\timing` | Activer/desactiver le chronometrage des requetes |
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
