---
title: Premiers pas avec Scala
description: Ajoutez PetraDB à votre projet Scala et exécutez vos premières requêtes SQL.
---

## Installation

Ajoutez à votre `build.sbt` :

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.2"
```

L'opérateur `%%%` sélectionne automatiquement le bon artefact pour votre plateforme — JVM, Scala.js ou Scala Native.

## Exécutez votre première requête

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

Chaque instance `MemoryDB` est une base de données en mémoire entièrement isolée. Toutes les données restent en mémoire — rien ne touche le système de fichiers.

## Stockage persistant

Lorsque vous avez besoin que les données survivent aux redémarrages, PetraDB propose deux options qui ne nécessitent aucune infrastructure externe :

**`PersistentDB`** — stockage durable résistant aux pannes dans un seul fichier, utilisant des pages en copie-sur-écriture et des en-têtes à double tampon via [stow](https://github.com/edadma/stow). Disponible sur JVM et Native.

**`TextDB`** — stocke la base de données sous forme de fichier `.ptxt` lisible par l'homme. Idéal pour le développement, les données de configuration et le contrôle de version.

Les deux sont couverts en détail dans le [guide Scala](/guides/scala/).

## Essayez-le dans le navigateur

Vous pouvez expérimenter le support SQL de PetraDB dès maintenant — sans aucune configuration de projet. Le [playground](/playground/) exécute le moteur complet dans votre navigateur.

## Étapes suivantes

Le [guide Scala](/guides/scala/) couvre les bases de données persistantes et texte, l'exécution SQL, la gestion des résultats et l'API complète. Pour exécuter PetraDB en tant que service réseau, consultez les guides [Serveur](/guides/server/) et [Client](/guides/client/).
