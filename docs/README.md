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
├── getting-started.md
├── guides/
│   ├── javascript.md
│   └── scala.md
└── reference/
    ├── api.md
    └── sql.md
```

## Deployment

Deployed automatically via GitHub Actions on push to `stable` when `docs/**` files change. See `.github/workflows/deploy-docs.yml`.
