# rdb improvement roadmap

## Fixed

### 1. ORDER BY can't evaluate aggregate expressions (fixed)

`SELECT department, COUNT(*) FROM emp GROUP BY department ORDER BY COUNT(*) DESC`

Fixed by making the pipeline conditional: grouped queries use PROJECT → HAVING → ORDER BY
with aggregate expressions resolved to projected column references via `resolveOrderBy`.

### 2. HAVING can't evaluate aggregates in compound boolean expressions (fixed)

`HAVING COUNT(*) > 1 AND SUM(salary) > 100000`

Fixed in two parts: (a) `beval` now accepts and passes through the aggregate mode instead
of hardcoding `AggregateMode.Disallow`, and (b) HAVING conditions are processed through
`resolveHaving` which recursively replaces aggregate sub-expressions with column references
to the projected output.

### 3. ORDER BY doesn't resolve SELECT aliases (fixed)

`SELECT department, COUNT(*) as cnt FROM emp GROUP BY department ORDER BY cnt DESC`

Fixed by the conditional pipeline: for grouped queries, ORDER BY runs after projection,
so aliases are visible as column names in the projected output.

### 4. HAVING without GROUP BY doesn't work (fixed)

`SELECT COUNT(*) FROM emp HAVING COUNT(*) > 3`

Fixed by the `isGrouped` detection which includes queries with aggregates in SELECT
(not just explicit GROUP BY). The `UngroupedProcess` handles accumulation, and
`resolveHaving` maps the HAVING condition's aggregates to projected columns.

## Not yet implemented

### 5. SELECT DISTINCT

The `DISTINCT` keyword is reserved in the parser and `DistinctProcess` exists in
Process.scala, but there is no parse rule to accept `SELECT DISTINCT ...`.

### 6. ORDER BY columns not in SELECT (with GROUP BY)

`SELECT department FROM emp GROUP BY department ORDER BY COUNT(*) DESC`

ORDER BY needs access to both the grouped columns and the ability to evaluate aggregates
that aren't in the SELECT list. Would require expanding the projection to include ORDER BY
expressions, sorting, then stripping the extra columns.
