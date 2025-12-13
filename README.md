# verbum-diei

Daily “Word of the Day” (Vatican News RSS) rendered as a static page with:
- scripture text from a public-domain translation (World English Bible via bible-api.com)
- LLM marginalia + line-referenced commentary (optional; runs once per day)
- liturgical observances (local calc via `romcal`)

## Setup

- Install deps: `bun install`
- Optional: set `OPENAI_API_KEY` to enable marginalia + commentary
- Defaults to `VERBUM_OPENAI_MODEL=gpt-5.2` (override with `VERBUM_OPENAI_MODEL` or `OPENAI_MODEL`)

## Generate today

- `bun run generate`
- Pick a specific date (YYYY-MM-DD): `bun run generate -- --date 2025-12-13`

Outputs:
- `data/YYYY-MM-DD.json`
- `public/index.html`
- `public/data/YYYY-MM-DD.json`
- `public/d/YYYY-MM-DD/index.html`
- `public/archive/index.html`

## Preview

- `bun run serve` then open `http://localhost:5173`

## Publish (GitHub Pages)

- Enable Pages with “GitHub Actions” as the source.
- `public/` deploys on push to `main` via `.github/workflows/pages.yml`.
