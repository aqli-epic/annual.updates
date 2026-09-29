# AQLI Annual Updates

This repository contains the source archive for figures, datasets, and code used in the [Air Quality Life Index (AQLI)](https://aqli.epic.uchicago.edu) annual reports, factsheets, and related analyses. It is maintained by the Energy Policy Institute at the University of Chicago.

> **Repository type:** Research archive. This is not an installable R package. Shared helpers live in `R/` and are sourced directly by figure scripts. `annual.updates.Rproj` does not build a package.

## Repository Structure

| Path | Contents |
|---|---|
| `2026/` | Current annual update |
| `2025/` | Previous annual update, including `2025/india-extract/` |
| `archive/2022`, `archive/2023`, `archive/2024` | Closed update years |
| `R/` | Shared helpers. `R/paths.R` resolves the repository root and external inputs |
| `man/` | Older roxygen notes for two helper functions. Not a package manual |

Generated figures and the tables they are built from stay in git when they are the published source. Large files kept for that reason include `archive/2023/factsheets/usa/fig1/fig1.svg` (48 MB), `archive/2023/factsheets/usa/fig2/fig2.svg` (26 MB), and the color tables in `archive/2022/master.dataset/` (`color_2020.csv` 21 MB, `color_2016.csv` 20 MB, `color_2019.csv` 17 MB).

## Reproducing a Figure

Figure 1.1 from the 2026 global section is used as the worked example below.

1. Open R with the working directory inside this repository. R 4.3.3 matches the app lockfiles.
2. The script is `2026/annual.report.2026/1.Global/fig1.1/script.R`.
3. It sources `~/R/helper_function_2026.R`, which is not in this repository. Place that helper on the machine, or set the script to source the copy your team uses.
4. The script expects a country-level table already loaded as `gadm0_aqli_2024` (columns `population` and `pm1998` through the latest `pm` year).
5. The published output next to the script is `ar_global_fig1.1.png`.

For the full 2026 map of scripts, helpers, and outputs, see `2026/README.md`.

External inputs that are not in git are read from `AQLI_DATA_DIR`. `R/july.2025.helper.script.R` still accepts the historical `~/Desktop/` filenames when `AQLI_DATA_DIR` is unset and those files are present. It does not call `setwd()`.

## Citation and License

Cite the annual update year you used. See `CITATION.cff`.

An open license has not been chosen. See `LICENSE`. Until AQLI publishes one, the code and the data are all rights reserved.

## Contributing

Folder conventions, naming rules for new work, and figure pull-request requirements are documented in `CONTRIBUTING.md`.
