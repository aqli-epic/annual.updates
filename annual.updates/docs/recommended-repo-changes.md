# Recommended changes: professional org standard for annual.updates

This note records a review of [aqli-epic/annual.updates](https://github.com/aqli-epic/annual.updates) as of 25 September 2026 (review started at commit `8da69d1` on `master`). The changes below have been applied in the working tree. An open license was not invented: `LICENSE` records all rights reserved until AQLI chooses one. `archive/` and the finished `2025/` figure tree were not renamed. `R/july.2024.helper.script.R` was left unchanged because the 2024 archive scripts source it and depend on its working-directory behavior.

The repository is a working research archive of AQLI figures, datasets, and code. The public face still reads like a private working folder: the README is out of date, package metadata is still the R template, folder names follow three conventions at once, and a few files that should stay off a public org repo are committed.

Historical figure scripts should keep their current paths. Standardization applies to the public docs, git hygiene, and new work.

## Current layout

| Path | What it is |
|---|---|
| `2025/`, `2026/` | Active annual-update years at the repo root |
| `archive/2022`, `archive/2023`, `archive/2024` | Closed update years |
| `R/`, `man/` | Shared helper scripts and two `roxygen2` help pages |
| `AQFund Analytic Dev/` | Shiny app, with its own `renv.lock` |
| `AQLI Analytics Explorer Dashboard/` | Shiny app, with its own `renv.lock` |
| `DataExplorerDash/` | Shiny app, with its own `renv.lock` |
| `FS Prompt Dev/` | Factsheet prompt draft (`.qmd` and `.docx`) |
| `India/` | Four India CSV extracts, not tied to a year folder |
| `DESCRIPTION`, `NAMESPACE`, `annual.updates.Rproj` | R package scaffold |

About 1,053 tracked files, roughly 683 MB. Outputs and inputs are committed together: about 351 PNGs, 220 CSVs, shapefiles, and a few multi‑megabyte SVGs and color tables.

The root README still says the current update is a `2024` folder. That folder is `archive/2024`. The README does not mention 2025, 2026, the dashboards, `India/`, or `FS Prompt Dev`.

## 1. Make the public face accurate

Replace the root `README.md` with a short org README that includes:

- What AQLI annual updates are, and who the repo is for
- A directory map that matches the tree on disk
- Where the current report year lives, and where older years live
- How to reproduce one figure: which script, which inputs, which output
- Links to the published report and factsheets

`DESCRIPTION` and `NAMESPACE` are still the R package template:

- `Package: annual.report.june.2022`
- `Title: What the Package Does (Title Case)`
- `Author: Who wrote it`
- `Maintainer: The package maintainer <yourself@somewhere.net>`
- `License: What license is it under?`
- `NAMESPACE` is `exportPattern("^[[:alpha:]]+")`, which exports every function

`annual.updates.Rproj` is set to `BuildType: Package`, but the helpers in `R/` are not an installable package. They call `setwd("~/Desktop/")` and read files that are not in the package. `man/` documents two functions. The year-specific helpers (`june.2022.helper.script.R`, `july.2024.helper.script.R`, `july.2025.helper.script.R`) are scripts, not package code.

Recommended package stance: stop presenting the whole repo as one installable package. Keep `R/` as shared helpers. Either give those helpers real metadata and make them install cleanly, or turn off package build settings until that is true.

## 2. Add the files an org repo is expected to have

None of these exist today.

| File | Purpose |
|---|---|
| `LICENSE` | How others may use the code and the data. Needs an explicit choice. Do not invent one. |
| `CITATION.cff` or `CITATION` | How to cite an annual update |
| `CONTRIBUTING.md` | Branch, folder, and figure-script rules for analysts |
| `.github/PULL_REQUEST_TEMPLATE.md` | What a figure PR must include: script, data, output, year |
| `.github/ISSUE_TEMPLATE/` | Bug, data-question, and new-figure templates |
| `CODEOWNERS` | Who reviews report, factsheet, and dashboard changes |

Open decision: whether the license covers code only, or code and the published datasets. Data licensing should be chosen by AQLI, not assumed.

## 3. One naming convention, applied going forward

Three conventions are in use at once.

| Pattern | Example |
|---|---|
| Lowercase, dotted | `2025/annual.report.2025/5.south.asia` |
| Title case | `2026/annual.report.2026/5.Latin.America` |
| Spaces in folder names | `AQFund Analytic Dev`, `AQLI Analytics Explorer Dashboard`, `FS Prompt Dev`, `2025/factsheets/latin america`, `helper functions` |

`2026/factsheets/` has no country folders yet. `2025/factsheets/` uses country names, with one space (`latin america`).

Document one convention and use it for new work:

- Years stay at the repo root: `2025/`, `2026/`. A completed year moves under `archive/` only when that cycle is closed.
- Inside a year: `annual-report/`, `factsheets/`, `website/`, plus any one-off analysis.
- Sections and figures: `01-global/fig-1-1/` — numeric prefix, lowercase, hyphens, no spaces.
- Apps live under `apps/aqfund/`, `apps/analytics-explorer/`, and `apps/data-explorer/`. Each app gets a README that says whether it is the current app.
- Shared code stays in `R/`.
- One-off extracts such as root `India/` move under the year they belong to.
- Prompt drafts move under something like `internal/factsheet-prompts/` so the root is only years, archive, shared code, apps, and docs.

Leave `archive/` and the finished 2025 tree on their current paths. Those paths are referenced by existing scripts. Renaming them would not make the repo more usable.

## 4. Remove files that should not be in a public org repo

Do this first. The repository is already public.

- Plaintext dashboard usernames and passwords are committed in `AQLI Analytics Explorer Dashboard/global.R` (the credentials data frame near the “AUTHENTICATION DATA” comment). Rotate those passwords, then delete them from the file. Read auth from environment variables.
- Four `.DS_Store` files are tracked: the repo root, `2026/`, `2026/annual.report.2026/`, and `2026/factsheets/`. `2026/factsheets/` contains only that `.DS_Store`.
- `rsconnect/` deploy records for shinyapps.io are committed in all three apps (`AQFund Analytic Dev`, `AQLI Analytics Explorer Dashboard`, `DataExplorerDash`). They name the `aqli-epic` account, app ids, and bundle ids. Stop tracking `rsconnect/`.

`.gitignore` today only ignores `.Rproj.user`, `.Rhistory`, `.RData`, `.Ruserdata`, and `.DS_Store`. `.DS_Store` is listed and still tracked, because ignore rules do not untrack files that are already committed.

Expand `.gitignore` to cover:

- `.DS_Store`
- `rsconnect/`
- `.Rproj.user/`
- `.Rhistory`, `.RData`, `.Ruserdata`
- Shiny test snapshots (`*_.new.png`, already ignored inside `AQFund Analytic Dev` only)

Then remove the tracked `.DS_Store` files and `rsconnect/` directories from the index.

Author emails and machine-specific paths are also committed inside `R/` (for example `setwd("~/Desktop/")` and reads of `~/Desktop/aqli_gadm2_2023.csv` in `R/july.2025.helper.script.R`). Treat those as reproducibility bugs in section 5, and drop personal machine paths from shared scripts.

## 5. Make a figure reproducible from the repo

Helper scripts point outside the repo:

- `R/july.2025.helper.script.R` sets the working directory to `~/Desktop/` and reads GADM CSV and shapefile paths on that Desktop.
- `R/june.2022.helper.script.R` reads `./june.2022/master.dataset/color_2020.csv` and related files. The on-disk path is `archive/2022/master.dataset/`.

Each Shiny app has its own `renv.lock`. Nothing at the root records the R version or which lockfile to use. There is no root test suite. `AQFund Analytic Dev/.gitignore` only ignores shinytest2 debug snapshots.

For the current year:

- Replace absolute paths with paths relative to the repo root.
- Add a root `renv.lock`, or keep one lockfile per app and say so in that app’s README. Record the R version either way.
- Add `2026/README.md` stating which input file produces which figure.

Generated outputs can stay in git when they are the published figure source. Say that in the README. Call out files that are large enough to deserve a reason for staying:

- `archive/2023/factsheets/usa/fig1/fig1.svg` (48 MB)
- `archive/2023/factsheets/usa/fig2/fig2.svg` (26 MB)
- `archive/2022/master.dataset/color_2020.csv` (21 MB), `color_2016.csv` (20 MB), `color_2019.csv` (17 MB)

## Suggested order

1. Secrets, `.gitignore`, and tracked `.DS_Store` / `rsconnect/` files.
2. README, license, citation, and contributing files.
3. Naming guide, then move only the loose root folders (`apps/`, the India extract, prompt drafts).
4. Path and `renv` cleanup for 2026. Leave archive scripts on their current paths.

## Out of scope for a first pass

- Rewriting figure scripts under `archive/` or finished `2025/` work.
- Choosing a license without an AQLI decision.
- GitHub settings that are not files: branch protection, required review, and repository topics. Those belong in a follow-up on the GitHub repo settings.
