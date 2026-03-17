# jeapostrophe.github.com

Personal website for Jay McCarthy. Built with a minimal Deno-based static site generator.

## Repository layout

```
content/          Markdown source files
  _home.md        Homepage (manually maintained list of links)
  tech/           Technical articles
  poetry/         Poems (subdirectories for series)
  games/          Retro/indie game writing
  faith/          Theological articles
static/           Legacy static content (old-blog/, home/, etc.)
build.ts          Static site generator (~300 lines)
style.css         Single classless CSS file
deno.json         Build tasks
.github/workflows/deploy.yml  GitHub Actions deployment
```

## Local development

Preview the site locally:

```sh
deno task serve
```

This builds to `_site/` and serves at `http://localhost:8000`.

To rebuild without serving:

```sh
deno task build
```

To rebuild automatically when content changes:

```sh
deno task watch
```

You can also open `_site/index.html` directly in a browser — all paths are relative.

## Deployment

Deployment is automatic via GitHub Actions. Push to `master` and the site is built and deployed to GitHub Pages.

### Initial GitHub setup

1. Go to the repo's **Settings > Pages**
2. Under **Build and deployment > Source**, select **GitHub Actions**
3. Push to `master` — the workflow will build and deploy automatically

## Content format

### Articles (timestamped, reverse-chronological)

```markdown
---
title: "Why Continuations Matter"
date: 2026-03-10
type: article
description: "Optional one-line summary"
---

Article body in Markdown.
```

### Pages (curated, manually ordered)

```markdown
---
title: "Reach"
type: page
weight: 10
url: https://github.com/reach-sh/reach-lang
description: "A programming language for blockchain safety"
---
```

Pages with `url` set are external links — they appear in section indexes but don't generate their own HTML page. Lower `weight` = more prominent.

### Poetry

```markdown
---
title: "Morning Prayer III"
date: 2026-02-15
type: article
format: poetry
series_order: 3
---

Lines are preserved
as written in the source.
```

Place poems in `content/poetry/series-name/` for series grouping. Add a `_series.md` file for series metadata:

```markdown
---
title: Morning Prayers
---

Optional series description.
```

## How sections and series work

- **Section indexes** (e.g., `/tech/`) are generated automatically when a section has 3+ content items
- **Series indexes** (e.g., `/poetry/morning-prayers/`) are generated when a series has 2+ items
- The homepage (`content/_home.md`) is manually maintained — add links as content grows
- Pages appear before articles in section indexes; pages sorted by weight, articles by date
