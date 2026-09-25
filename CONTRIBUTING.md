# Contributing to annual.updates

This repository is the source archive for AQLI annual reports, factsheets, and related figures. Changes land through a pull request.

## Where work goes

- The current update year stays at the repository root (`2026/` while that cycle is open).
- A closed year moves under `archive/` only after that cycle is finished.
- Leave `archive/` and the finished `2025/` tree on their current paths. Figure scripts in those trees point at those paths.
- Shared helpers stay in `R/`.
- Shiny apps stay in `apps/`.
- Draft prompts stay in `internal/factsheet-prompts/`.

## Naming for new work

Use this convention for new folders. Do not rename historical folders to match it.

- Inside a year: `annual-report/`, `factsheets/`, `website/`, plus a clearly named one-off analysis folder.
- Sections and figures: `01-global/fig-1-1/`. Numeric prefix, lowercase, hyphens, no spaces.
- Factsheet countries: lowercase, hyphens (`latin-america`, not `latin america`).
- Apps: `apps/<short-name>/`, with a README that says whether the app is current.

The 2025 tree uses lowercase dotted names (`5.south.asia`). The 2026 tree uses title case (`5.Latin.America`). Both stay as they are until a new year starts.

## What a figure change includes

- The script that builds the figure.
- The input table, if it is part of the published source and is small enough to commit.
- The output figure (PNG, SVG, or PDF) when that file is the published source.
- A line in the year README when a new figure is added.

Do not commit `.DS_Store`, `rsconnect/`, R session files, or passwords.

## Paths

- Read repository files with paths relative to the repository root. `R/paths.R` resolves that root from the working directory.
- External inputs (GADM extracts and shapefiles that are not in git) go in a directory named by the `AQLI_DATA_DIR` environment variable.
- Do not add `setwd()` or a personal Desktop path to shared helpers.

## Apps

Each app has its own `renv.lock`. The lockfiles in this repository were built with R 4.3.3. There is no root lockfile.

`apps/analytics-explorer` reads dashboard logins from `AQLI_DASHBOARD_USERS` and `AQLI_DASHBOARD_PASSWORDS`. Do not commit those values. Rotate any password that was previously stored in `global.R`.

## Pull requests

Use the pull request template. Say which year and figure changed, and how you checked the script.
