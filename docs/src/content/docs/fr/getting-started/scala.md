---
title: Premiers pas avec Scala
description: Ajoutez PetraDB a votre projet Scala et executez vos premieres requetes SQL.
---

## Installation

Ajoutez a votre `build.sbt` :

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.0"
```

L'operateur `%%%` selectionne automatiquement le bon artefact pour votre plateforme -- JVM, Scala.js ou Scala Native.

## Executez votre premiere requete

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');

  SELECT * FROM users;
""")

results.foreach(println)
```

Chaque instance `MemoryDB` est une base de donnees en memoire entierement isolee. Toutes les donnees restent en memoire -- rien ne touche le systeme de fichiers.

## Stockage persistant

Lorsque vous avez besoin que les donnees survivent aux redemarrages, PetraDB propose deux options qui ne necessitent aucune infrastructure externe :

**`PersistentDB`** -- stockage durable resistant aux pannes dans un seul fichier, utilisant des pages en copie-sur-ecriture et des en-tetes a double tampon via [stow](https://github.com/edadma/stow). Disponible sur JVM et Native.

**`TextDB`** -- stocke la base de donnees sous forme de fichier `.ptxt` lisible par l'homme. Ideal pour le developpement, les donnees de configuration et le controle de version.

Les deux sont couverts en detail dans le [guide Scala](/guides/scala/).

## Essayez-le dans le navigateur

Vous pouvez experimenter le support SQL de PetraDB des maintenant -- sans aucune configuration de projet. Le [playground](/playground/) execute le moteur complet dans votre navigateur.

## Etapes suivantes

Le [guide Scala](/guides/scala/) couvre les bases de donnees persistantes et texte, l'execution SQL, la gestion des resultats et l'API complete. Pour executer PetraDB en tant que service reseau, consultez les guides [Serveur](/guides/server/) et [Client](/guides/client/).
