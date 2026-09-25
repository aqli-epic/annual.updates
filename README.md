# AQLI annual updates

Source archive for the figures, datasets, and code behind the [Air Quality Life Index](https://aqli.epic.uchicago.edu) annual reports, factsheets, and related analyses. It is maintained by the Energy Policy Institute at the University of Chicago.

This is a research archive. It is not an installable R package. Shared helpers live in `R/` and are sourced by figure scripts. `annual.updates.Rproj` does not build a package.

## Repository map

| Path | Contents |
|---|---|
| `2026/` | Current annual update |
| `2025/` | Previous annual update, including `2025/india-extract/` |
| `archive/2022`, `archive/2023`, `archive/2024` | Closed update years |
| `R/` | Shared helpers. `R/paths.R` resolves the repository root and external inputs |
| `apps/aqfund/` | AQ Fund Shiny app. Development app |
| `apps/analytics-explorer/` | Analytics Explorer Shiny app. Current dashboard |
| `apps/data-explorer/` | Earlier data-explorer Shiny app |
| `internal/factsheet-prompts/` | Factsheet prompt drafts |
| `man/` | Older roxygen notes for two helper functions. Not a package manual |

Generated figures and the tables they are built from stay in git when they are the published source. Large files kept for that reason include `archive/2023/factsheets/usa/fig1/fig1.svg` (48 MB), `archive/2023/factsheets/usa/fig2/fig2.svg` (26 MB), and the color tables in `archive/2022/master.dataset/` (`color_2020.csv` 21 MB, `color_2016.csv` 20 MB, `color_2019.csv` 17 MB).

## Reproduce one figure

Figure 1.1 of the 2026 global section is the worked example.

1. Open R with the working directory inside this repository. R 4.3.3 matches the app lockfiles.
2. The script is `2026/annual.report.2026/1.Global/fig1.1/script.R`.
3. It sources `~/R/helper_function_2026.R`, which is not in this repository. Place that helper on the machine, or set the script to source the copy your team uses.
4. The script expects a country-level table already loaded as `gadm0_aqli_2024` (columns `population` and `pm1998` through the latest `pm` year).
5. The published output next to the script is `ar_global_fig1.1.png`.

The full 2026 map of scripts, helpers, and outputs is in `2026/README.md`.

External inputs that are not in git are read from `AQLI_DATA_DIR`. `R/july.2025.helper.script.R` still accepts the historical `~/Desktop/` filenames when `AQLI_DATA_DIR` is unset and those files are present. It does not call `setwd()`.

## R environment

There is no root `renv.lock`. Each app pins its own library:

| App | Lockfile | R version recorded in the lockfile |
|---|---|---|
| `apps/aqfund/` | `renv.lock` | 4.3.3 |
| `apps/analytics-explorer/` | `renv.lock` | 4.3.3 |
| `apps/data-explorer/` | `renv.lock` | 4.3.3 |

From an app directory, restore with `renv::restore()` after opening that app's project.

`apps/analytics-explorer` will not start until `AQLI_DASHBOARD_USERS` and `AQLI_DASHBOARD_PASSWORDS` are set to comma-separated lists of the same length. Rotate any password that was previously committed in `global.R` before setting the new values.

## Citation and license

Cite the annual update year you used. See `CITATION.cff`.

An open license has not been chosen. See `LICENSE`. Until AQLI publishes one, the code and the data are all rights reserved.

## Contributing

Folder rules, the naming convention for new work, and what a figure pull request includes are in `CONTRIBUTING.md`.
