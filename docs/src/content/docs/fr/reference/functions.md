---
title: Fonctions
description: Référence des fonctions scalaires et d'agrégation.
---

## Fonctions texte

| Fonction | Description |
|----------|-------------|
| `lower(text)` | Convertir en minuscules |
| `upper(text)` | Convertir en majuscules |
| `initcap(text)` | Mettre en majuscule chaque mot |
| `length(text)` / `char_length(text)` | Longueur de la chaîne |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Supprimer les espaces |
| `btrim(text [, chars])` | Supprimer des caractères des deux côtés |
| `substring(text, start [, len])` | Extraire une sous-chaîne |
| `left(text, n)` / `right(text, n)` | Premiers/derniers n caractères |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Compléter la chaîne |
| `replace(text, from, to)` | Remplacer les occurrences |
| `translate(text, from, to)` | Substitution caractère par caractère |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concaténer (avec séparateur) |
| `repeat(text, n)` | Répéter la chaîne |
| `reverse(text)` | Inverser la chaîne |
| `position(substr, text)` | Trouver la position de la sous-chaîne (base 1) |
| `split_part(text, delim, n)` | Découper et obtenir la n-ième partie |
| `ascii(text)` / `chr(int)` | Conversion caractère/point de code |
| `regexp_replace(text, pat, repl [, flags])` | Remplacement par regex (`'g'` pour global) |
| `regexp_match(text, pattern)` | Première correspondance regex sous forme de tableau |
| `regexp_split_to_array(text, pattern)` | Découper par regex en tableau |
| `starts_with(text, prefix)` | Vrai si le texte commence par le préfixe |
| `ends_with(text, suffix)` | Vrai si le texte se termine par le suffixe |
| `format(formatstr, ...)` | Formater une chaîne (voir ci-dessous) |
| `quote_ident(value)` | Quoter comme identifiant SQL |
| `quote_literal(value)` | Quoter comme littéral SQL |

### format()

Supporte les spécificateurs de format suivants :

| Spécificateur | Description |
|---------------|-------------|
| `%s` | Substitution de chaîne |
| `%I` | Identifiant SQL (quoté et échappé) |
| `%L` | Littéral SQL (quoté et échappé) |
| `%%` | Signe pourcentage littéral |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## Fonctions numériques

| Fonction | Description |
|----------|-------------|
| `abs(x)` | Valeur absolue |
| `ceil(x)` / `floor(x)` | Arrondir vers le haut/bas |
| `round(x [, digits])` / `trunc(x [, digits])` | Arrondir/tronquer |
| `sign(x)` | Signe (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Puissance/racine carrée |
| `cbrt(x)` | Racine cubique |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponentielle/logarithme |
| `div(x, y)` | Division entière |
| `factorial(n)` | Factorielle |
| `gcd(a, b)` / `lcm(a, b)` | Plus grand diviseur commun / plus petit multiple commun |
| `pi()` | Constante Pi |
| `degrees(rad)` / `radians(deg)` | Conversion d'angles |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonométrie |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Trigonométrie hyperbolique |
| `random()` | Nombre aléatoire [0, 1) |
| `setseed(seed)` | Initialiser le générateur de nombres aléatoires |
| `width_bucket(value, low, high, count)` | Assigner une valeur à un compartiment (voir ci-dessous) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Max/min des valeurs |

### Opérateurs bit à bit

| Opérateur | Description |
|-----------|-------------|
| `x % y` | Modulo |
| `x & y` | ET bit à bit |
| `x \| y` | OU bit à bit |
| `x # y` | OU exclusif bit à bit |
| `~x` | NON bit à bit |
| `x << n` | Décalage à gauche |
| `x >> n` | Décalage à droite |

### width_bucket()

Assigne une valeur à l'un des `count` compartiments de largeur égale dans l'intervalle `[low, high)` :

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (compartiment pour les valeurs 30-39)
```

Retourne 0 pour les valeurs en dessous de `low`, et `count + 1` pour les valeurs à ou au-dessus de `high`.

## Fonctions date/heure

| Fonction | Description |
|----------|-------------|
| `now()` | Horodatage actuel (UTC) |
| `clock_timestamp()` | Horodatage actuel (UTC) |
| `current_date()` | Date actuelle (UTC) |
| `current_time()` | Heure actuelle (UTC) |
| `date_part(field, source)` | Extraire un champ d'une date/heure |
| `EXTRACT(field FROM source)` | Extraction standard SQL |
| `date_trunc(field, source)` | Tronquer à la précision (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construire une date/heure |
| `make_timestamp(y, mo, d, h, mi, s)` | Construire un horodatage |
| `make_interval(days [, hours [, mins [, secs]]])` | Construire un intervalle |
| `age(ts1, ts2)` / `age(ts)` | Intervalle entre deux horodatages |
| `to_char(value, format)` | Formater en texte |
| `to_date(text, format)` / `to_timestamp(text, format)` | Analyser avec un format |
| `to_number(text, format)` | Analyser une chaîne numérique |
| `isfinite(date\|timestamp)` | Toujours vrai (pas d'infinis dans le temps Java) |

## Fonctions de tableaux

| Fonction | Description |
|----------|-------------|
| `array_length(arr)` | Nombre d'éléments |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Ajouter un élément |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | Concaténer des tableaux |
| `array_slice(arr, start [, end])` | Extraire une portion du tableau |
| `array_remove(arr, val)` | Supprimer toutes les occurrences |
| `array_position(arr, val)` | Trouver la position d'un élément (base 1) |
| `array_distinct(arr)` | Supprimer les doublons |
| `array_replace(arr, old, new)` | Remplacer les éléments correspondants |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Bornes du tableau (base 1) |
| `array_ndims(arr)` | Nombre de dimensions (toujours 1) |
| `cardinality(arr)` | Nombre d'éléments |
| `string_to_array(text, delim)` | Découper une chaîne en tableau |
| `array_to_string(arr, sep)` | Joindre un tableau en chaîne |

## Fonctions binaires

| Fonction | Description |
|----------|-------------|
| `octet_length(bytea)` | Nombre d'octets |
| `get_byte(bytea, offset)` | Obtenir l'octet au décalage base 0 (retourne 0-255) |
| `set_byte(bytea, offset, value)` | Définir l'octet au décalage, retourne un nouveau bytea |
| `encode(bytea, format)` / `decode(text, format)` | Encodage binaire (hex, base64) |

## Fonctions de séquence

| Fonction | Description |
|----------|-------------|
| `nextval('name')` | Avancer la séquence et retourner la valeur suivante |
| `currval('name')` | Valeur actuelle (nécessite un `nextval` préalable dans la session) |
| `setval('name', value [, is_called])` | Définir la valeur de la séquence (`is_called` par défaut à `true`) |
| `lastval()` | Dernière valeur retournée par toute séquence dans cette session |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## Autres fonctions scalaires

| Fonction | Description |
|----------|-------------|
| `coalesce(a, b, ...)` | Première valeur non-null |
| `nullif(a, b)` | NULL si a = b |
| `typeof(value)` | Nom du type sous forme de texte |
| `gen_random_uuid()` | Générer un UUID v4 |

## Fonctions d'agrégation

| Fonction | Description |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Compter les lignes |
| `SUM(expr)` | Somme des valeurs |
| `AVG(expr)` | Moyenne |
| `MIN(expr)` / `MAX(expr)` | Minimum/maximum |
| `string_agg(text, separator)` | Concaténer avec un séparateur |
| `array_agg(expr)` | Collecter les valeurs dans un tableau |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | ET/OU logique sur les lignes |
| `bit_and(expr)` | ET bit à bit sur les lignes |
| `bit_or(expr)` | OU bit à bit sur les lignes |
| `bit_xor(expr)` | OU exclusif bit à bit sur les lignes |
| `variance(expr)` / `var_samp(expr)` | Variance échantillon |
| `var_pop(expr)` | Variance de population |
| `stddev(expr)` / `stddev_samp(expr)` | Écart type échantillon |
| `stddev_pop(expr)` | Écart type de population |

Toutes les fonctions d'agrégation supportent la clause `FILTER (WHERE ...)` pour restreindre les lignes incluses :

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

Voir aussi : [Fonctions d'agrégation JSON](/reference/json/#aggregate-functions)

## Fonctions de fenêtre

Les fonctions de fenêtre calculent une valeur pour chaque ligne basée sur un groupe de lignes liées, sans les regrouper.

### Fonctions de classement

| Fonction | Description |
|----------|-------------|
| `ROW_NUMBER()` | Numéro de ligne séquentiel dans la partition |
| `RANK()` | Rang avec sauts pour les égalités |
| `DENSE_RANK()` | Rang sans sauts pour les égalités |

### Fonctions de décalage

| Fonction | Description |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | Valeur d'une ligne précédente (décalage par défaut : 1) |
| `LEAD(expr [, offset [, default]])` | Valeur d'une ligne suivante (décalage par défaut : 1) |
| `NTILE(n)` | Diviser les lignes en n groupes à peu près égaux |

### Fonctions de valeur

| Fonction | Description |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valeur de `expr` à la première ligne du cadre de fenêtre |
| `LAST_VALUE(expr)` | Valeur de `expr` à la dernière ligne du cadre de fenêtre |
| `NTH_VALUE(expr, n)` | Valeur de `expr` à la n-ième ligne du cadre (base 1), ou NULL si pas de telle ligne |

### Fonctions de fenêtre avec agrégats

Toute fonction d'agrégation peut être utilisée comme fonction de fenêtre avec `OVER()`. Voir [Requêtes — Fonctions de fenêtre](/reference/queries/#window-functions) pour la syntaxe et les spécifications de cadre.
