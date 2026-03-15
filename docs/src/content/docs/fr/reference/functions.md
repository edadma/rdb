---
title: Fonctions
description: Reference des fonctions scalaires et d'agregation.
---

## Fonctions texte

| Fonction | Description |
|----------|-------------|
| `lower(text)` | Convertir en minuscules |
| `upper(text)` | Convertir en majuscules |
| `initcap(text)` | Mettre en majuscule chaque mot |
| `length(text)` / `char_length(text)` | Longueur de la chaine |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Supprimer les espaces |
| `btrim(text [, chars])` | Supprimer des caracteres des deux cotes |
| `substring(text, start [, len])` | Extraire une sous-chaine |
| `left(text, n)` / `right(text, n)` | Premiers/derniers n caracteres |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Completer la chaine |
| `replace(text, from, to)` | Remplacer les occurrences |
| `translate(text, from, to)` | Substitution caractere par caractere |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concatener (avec separateur) |
| `repeat(text, n)` | Repeter la chaine |
| `reverse(text)` | Inverser la chaine |
| `position(substr, text)` | Trouver la position de la sous-chaine (base 1) |
| `split_part(text, delim, n)` | Decouper et obtenir la n-ieme partie |
| `ascii(text)` / `chr(int)` | Conversion caractere/point de code |
| `regexp_replace(text, pat, repl [, flags])` | Remplacement par regex (`'g'` pour global) |
| `regexp_match(text, pattern)` | Premiere correspondance regex sous forme de tableau |
| `regexp_split_to_array(text, pattern)` | Decouper par regex en tableau |
| `starts_with(text, prefix)` | Vrai si le texte commence par le prefixe |
| `ends_with(text, suffix)` | Vrai si le texte se termine par le suffixe |
| `format(formatstr, ...)` | Formater une chaine (voir ci-dessous) |
| `quote_ident(value)` | Citer comme identifiant SQL |
| `quote_literal(value)` | Citer comme litteral SQL |

### format()

Supporte les specificateurs de format suivants :

| Specificateur | Description |
|---------------|-------------|
| `%s` | Substitution de chaine |
| `%I` | Identifiant SQL (cite et echappe) |
| `%L` | Litteral SQL (cite et echappe) |
| `%%` | Signe pourcentage litteral |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## Fonctions numeriques

| Fonction | Description |
|----------|-------------|
| `abs(x)` | Valeur absolue |
| `ceil(x)` / `floor(x)` | Arrondir vers le haut/bas |
| `round(x [, digits])` / `trunc(x [, digits])` | Arrondir/tronquer |
| `sign(x)` | Signe (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Puissance/racine carree |
| `cbrt(x)` | Racine cubique |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponentielle/logarithme |
| `div(x, y)` | Division entiere |
| `factorial(n)` | Factorielle |
| `gcd(a, b)` / `lcm(a, b)` | Plus grand diviseur commun / plus petit multiple commun |
| `pi()` | Constante Pi |
| `degrees(rad)` / `radians(deg)` | Conversion d'angles |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonometrie |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Trigonometrie hyperbolique |
| `random()` | Nombre aleatoire [0, 1) |
| `setseed(seed)` | Initialiser le generateur de nombres aleatoires |
| `width_bucket(value, low, high, count)` | Assigner une valeur a un compartiment (voir ci-dessous) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Max/min des valeurs |

### Operateurs bit a bit

| Operateur | Description |
|-----------|-------------|
| `x % y` | Modulo |
| `x & y` | ET bit a bit |
| `x \| y` | OU bit a bit |
| `x # y` | OU exclusif bit a bit |
| `~x` | NON bit a bit |
| `x << n` | Decalage a gauche |
| `x >> n` | Decalage a droite |

### width_bucket()

Assigne une valeur a l'un des `count` compartiments de largeur egale dans l'intervalle `[low, high)` :

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (compartiment pour les valeurs 30-39)
```

Retourne 0 pour les valeurs en dessous de `low`, et `count + 1` pour les valeurs a ou au-dessus de `high`.

## Fonctions date/heure

| Fonction | Description |
|----------|-------------|
| `now()` | Horodatage actuel (UTC) |
| `clock_timestamp()` | Horodatage actuel (UTC) |
| `current_date()` | Date actuelle (UTC) |
| `current_time()` | Heure actuelle (UTC) |
| `date_part(field, source)` | Extraire un champ d'une date/heure |
| `EXTRACT(field FROM source)` | Extraction standard SQL |
| `date_trunc(field, source)` | Tronquer a la precision (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construire une date/heure |
| `make_timestamp(y, mo, d, h, mi, s)` | Construire un horodatage |
| `make_interval(days [, hours [, mins [, secs]]])` | Construire un intervalle |
| `age(ts1, ts2)` / `age(ts)` | Intervalle entre deux horodatages |
| `to_char(value, format)` | Formater en texte |
| `to_date(text, format)` / `to_timestamp(text, format)` | Analyser avec un format |
| `to_number(text, format)` | Analyser une chaine numerique |
| `isfinite(date\|timestamp)` | Toujours vrai (pas d'infinis dans le temps Java) |

## Fonctions de tableaux

| Fonction | Description |
|----------|-------------|
| `array_length(arr)` | Nombre d'elements |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Ajouter un element |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | Concatener des tableaux |
| `array_slice(arr, start [, end])` | Extraire une portion du tableau |
| `array_remove(arr, val)` | Supprimer toutes les occurrences |
| `array_position(arr, val)` | Trouver la position d'un element (base 1) |
| `array_distinct(arr)` | Supprimer les doublons |
| `array_replace(arr, old, new)` | Remplacer les elements correspondants |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Bornes du tableau (base 1) |
| `array_ndims(arr)` | Nombre de dimensions (toujours 1) |
| `cardinality(arr)` | Nombre d'elements |
| `string_to_array(text, delim)` | Decouper une chaine en tableau |
| `array_to_string(arr, sep)` | Joindre un tableau en chaine |

## Fonctions binaires

| Fonction | Description |
|----------|-------------|
| `octet_length(bytea)` | Nombre d'octets |
| `get_byte(bytea, offset)` | Obtenir l'octet au decalage base 0 (retourne 0-255) |
| `set_byte(bytea, offset, value)` | Definir l'octet au decalage, retourne un nouveau bytea |
| `encode(bytea, format)` / `decode(text, format)` | Encodage binaire (hex, base64) |

## Fonctions de sequence

| Fonction | Description |
|----------|-------------|
| `nextval('name')` | Avancer la sequence et retourner la valeur suivante |
| `currval('name')` | Valeur actuelle (necessite un `nextval` prealable dans la session) |
| `setval('name', value [, is_called])` | Definir la valeur de la sequence (`is_called` par defaut a `true`) |
| `lastval()` | Derniere valeur retournee par toute sequence dans cette session |

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
| `coalesce(a, b, ...)` | Premiere valeur non-null |
| `nullif(a, b)` | NULL si a = b |
| `typeof(value)` | Nom du type sous forme de texte |
| `gen_random_uuid()` | Generer un UUID v4 |

## Fonctions d'agregation

| Fonction | Description |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Compter les lignes |
| `SUM(expr)` | Somme des valeurs |
| `AVG(expr)` | Moyenne |
| `MIN(expr)` / `MAX(expr)` | Minimum/maximum |
| `string_agg(text, separator)` | Concatener avec un separateur |
| `array_agg(expr)` | Collecter les valeurs dans un tableau |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | ET/OU logique sur les lignes |
| `bit_and(expr)` | ET bit a bit sur les lignes |
| `bit_or(expr)` | OU bit a bit sur les lignes |
| `bit_xor(expr)` | OU exclusif bit a bit sur les lignes |
| `variance(expr)` / `var_samp(expr)` | Variance echantillon |
| `var_pop(expr)` | Variance de population |
| `stddev(expr)` / `stddev_samp(expr)` | Ecart type echantillon |
| `stddev_pop(expr)` | Ecart type de population |

Toutes les fonctions d'agregation supportent la clause `FILTER (WHERE ...)` pour restreindre les lignes incluses :

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

Voir aussi : [Fonctions d'agregation JSON](/reference/json/#aggregate-functions)

## Fonctions de fenetre

Les fonctions de fenetre calculent une valeur pour chaque ligne basee sur un groupe de lignes liees, sans les regrouper.

### Fonctions de classement

| Fonction | Description |
|----------|-------------|
| `ROW_NUMBER()` | Numero de ligne sequentiel dans la partition |
| `RANK()` | Rang avec sauts pour les egalites |
| `DENSE_RANK()` | Rang sans sauts pour les egalites |

### Fonctions de decalage

| Fonction | Description |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | Valeur d'une ligne precedente (decalage par defaut : 1) |
| `LEAD(expr [, offset [, default]])` | Valeur d'une ligne suivante (decalage par defaut : 1) |
| `NTILE(n)` | Diviser les lignes en n groupes a peu pres egaux |

### Fonctions de valeur

| Fonction | Description |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valeur de `expr` a la premiere ligne du cadre de fenetre |
| `LAST_VALUE(expr)` | Valeur de `expr` a la derniere ligne du cadre de fenetre |
| `NTH_VALUE(expr, n)` | Valeur de `expr` a la n-ieme ligne du cadre (base 1), ou NULL si pas de telle ligne |

### Fonctions de fenetre avec agregats

Toute fonction d'agregation peut etre utilisee comme fonction de fenetre avec `OVER()`. Voir [Requetes -- Fonctions de fenetre](/reference/queries/#window-functions) pour la syntaxe et les specifications de cadre.
