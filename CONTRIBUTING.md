# Contributing to annual.updates

This repository is the source archive for AQLI annual reports, factsheets, and related figures. Changes should be made through a pull request.

## Where work goes

- `2025/` is the latest completed annual update and remains at the repository root.
- `2026/` is the active work-in-progress annual update.
- Older closed update years are stored under `archive/`.
- Do not move or rename historical year directories unless the change is coordinated across all dependent scripts and documentation.
- Shared helpers stay in `R/`.

## Naming for new work

Use these conventions for new folders and files. Do not rename historical folders solely to match the current convention.

- Inside a year: `annual-report/`, `factsheets/`, `website/`, plus a clearly named one-off analysis folder when needed.
- Sections and figures: `01-global/fig-1-1/`.
- Use numeric prefixes where ordering matters.
- Use lowercase names with hyphens for new folders; avoid spaces.
- Factsheet regions and countries should use lowercase, hyphenated names, for example `latin-america`.

Historical naming conventions may differ. Existing historical paths should remain unchanged unless there is a specific migration plan.

## What a figure change includes

A figure-related pull request should include, where applicable:

- The script that builds the figure.
- The input table when it is part of the published source and is appropriate to keep in git.
- The output figure (PNG, SVG, or PDF) when that file is the published source.
- An update to the relevant year README when a new figure is added or an existing figure changes materially.

Do not commit `.DS_Store`, `rsconnect/`, R session files, passwords, credentials, or other machine-specific files.

## Paths

- Read repository files using paths relative to the repository root.
- `R/paths.R` resolves the repository root from the working directory.
- External inputs that are not stored in git should be placed in a directory referenced by the `AQLI_DATA_DIR` environment variable.
- Do not add `setwd()` calls or personal Desktop paths to shared code.
- Shared helper functions should be sourced from the repository rather than from user-specific filesystem locations.

## Pull requests

Use the pull request template.

A pull request should state:

- The annual-update year affected.
- The figure, factsheet, or analysis changed.
- The source data or inputs used.
- How the output was reproduced or checked.
- Any path, dependency, or output changes reviewers should know about.

Keep pull requests focused. Avoid combining unrelated refactors, figure changes, and data updates in the same pull request unless they are required for the same reproducible change.
