# PetraDB Documentation

Source for the documentation site at [petradb.dev](https://petradb.dev).

Built with [Astro Starlight](https://starlight.astro.build/).

## Development

```bash
cd docs
npm install
npm run dev
```

Opens a local dev server at `localhost:4321`.

## Structure

Doc pages live in `src/content/docs/`:

```
src/content/docs/
├── index.mdx
├── getting-started/
│   ├── javascript.md
│   └── scala.md
├── guides/
│   ├── cli.md
│   ├── client.md
│   ├── javascript.md
│   ├── knex.md
│   ├── scala.md
│   └── server.md
└── reference/
    ├── api-javascript.md
    ├── api-jdbc.md
    ├── api-scala.md
    ├── data-types.md
    ├── ddl.md
    ├── dml.md
    ├── functions.md
    ├── json.md
    ├── queries.md
    └── transactions.md
```

## Deployment

Deployed automatically via GitHub Actions on push to `stable` when `docs/**` files change. See `.github/workflows/deploy-docs.yml`.
