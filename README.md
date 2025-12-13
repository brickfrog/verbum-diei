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
- `public/index.html`
- `public/data/YYYY-MM-DD.json`
- `public/d/YYYY-MM-DD/index.html`
- `public/archive/index.html`

## Preview

- `bun run serve` then open `http://localhost:5173`

## Publish (GitHub Pages)

- Create a repository secret `OPENAI_API_KEY` (optional; only needed for LLM output).
- Run the workflow once (`Actions` → `Daily generate` → `Run workflow`) to create/update the `gh-pages` branch.
- Configure Pages to deploy from the `gh-pages` branch (root).
- `.github/workflows/daily.yml` runs daily, generates the site, and publishes `public/` to `gh-pages` (keeping `main` free of generated artifacts).
