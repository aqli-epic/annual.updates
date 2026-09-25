# 2026 annual update

Current AQLI update cycle. Chapter folders use the names already in this tree (`1.Global`, `5.Latin.America`, and so on). New folders follow the convention in `CONTRIBUTING.md`. Do not rename these chapters in place.

`factsheets/` is ready for country folders. None have been added yet.

National annual PM2.5 standards live in `national_PM2.5_standards_database/`. See that folder's readme before editing the CSV. Report corrections with the "Update National Standard" issue template.

## How a figure is built

Most scripts source a helper that is not stored in this repository:

- `~/R/july.2026.helper.script.R` loads the objects those chapters use, including `gadm0_aqli_2024` where a script uses it.
- `~/R/helper_function_2026.R` is the other helper (global, China, Europe figure 9.1, United States and Canada, regional differences, appendix).
- Europe figure 9.2 sources `R/july.2025.helper.script.R` from this repository.

Put external helpers on the machine, or point the script at your copy. External data files belong in `AQLI_DATA_DIR`. Figure 1.3 reads `treecover_loss_from_fires_global.csv` from that directory, and falls back to `~/treecover_loss_from_fires_global.csv` if the file is still there.

Run a script from this repository so `R/paths.R` can find the root. The published PNG next to each script is the figure source.

## Figures

| Section | Script | Output | Helper |
|---|---|---|---|
| 1.Global / fig1.1 | `script.R` | `ar_global_fig1.1.png` | `~/R/helper_function_2026.R` (path in the script is `~R/helper_function_2026.R`) |
| 1.Global / fig1.2 | `script.R` | `ar_global_fig1.2.png` | `~/R/july.2026.helper.script.R` |
| 1.Global / fig1.3 | `script.R` | `ar_global_fig1.3.png` | `~/R/july.2026.helper.script.R` plus `treecover_loss_from_fires_global.csv` |
| 2.Regional.differences | `script.R` | `ar_regional_fig2.1.png` | `~/R/helper_function_2026.R` |
| 3.South.Asia / fig3.1 | `script.R` | `ar_south_asia_fig3.1.png` | `~/R/july.2026.helper.script.R` |
| 3.South.Asia / fig3.2 | `script.R` | `ar_south_asia_fig3.2.png` | `~/R/july.2026.helper.script.R` |
| 3.South.Asia / fig3.3 | `script.R` | `ar_south_asia_fig3.3.png` | `~/R/july.2026.helper.script.R` |
| 4.Central.and.West.Africa / fig4.1 | `ar_cw_africa_fig4.1.R` | `ar_cw_africa_fig4.1.png` | `~/R/july.2026.helper.script.R` |
| 4.Central.and.West.Africa / fig4.2 | `script.R` | `ar_cw_africa_fig4.2.png` | `~/R/july.2026.helper.script.R` |
| 4.Central.and.West.Africa / fig4.3 | `script.R` | `ar_cw_africa_fig4.3.png` | `~/R/july.2026.helper.script.R` |
| 5.Latin.America / fig5.1 | `script.R` | `ar_latam_fig5.1.png` | `~/R/july.2026.helper.script.R` |
| 5.Latin.America / fig5.2 | `script.R` | `ar_latam_fig5.2.png` | `~/R/july.2026.helper.script.R` |
| 5.Latin.America / fig5.3 | `script.R` | `ar_latam_fig5.3.png` | `~/R/july.2026.helper.script.R` |
| 6.South.East.Asia / fig6.1 | `script.R` | `ar_se_asia_fig6.1.png` | `~/R/july.2026.helper.script.R` |
| 6.South.East.Asia / fig6.2 | `script.R` | `ar_se_asia_fig6.2.png` | `~/R/july.2026.helper.script.R` |
| 6.South.East.Asia / fig6.3 | `script.R` | `se_asia_fig6.3.png` | `~/R/july.2026.helper.script.R` |
| 7.MENA / fig7.1 | `script.R` | `ar_mena_fig7.1.png` | `~/R/july.2026.helper.script.R` |
| 7.MENA / fig7.2 | `script.R` | `ar_mena_fig7.2.png` | `~/R/july.2026.helper.script.R` |
| 7.MENA / fig7.3 | `script.R` | `ar_MENA_fig7.3.png` | `~/R/july.2026.helper.script.R` |
| 8.China / fig8.1 | `script.R` | `ar_china_fig8.1.png` | `~/R/helper_function_2026.R` |
| 8.China / fig8.2 | `script.R` | `ar_china_fig8.2.png` | `~/R/helper_function_2026.R` |
| 8.China / fig8.3 | `script.R` | `ar_chinal_fig8.3.png` | `~/R/helper_function_2026.R` |
| 9.Europe / fig9.1 | `script.R` | `ar_eur_fig9.1_data.png` | `~/R/helper_function_2026.R` |
| 9.Europe / fig9.2 | `script.R` | `ar_europe_fig9.2.png` | `R/july.2025.helper.script.R` |
| 10.US.Canada / fig10.1 | `script.R` | `ar_us_canada_fig10.1.png` | `~/R/helper_function_2026.R` |
| 10.US.Canada / fig10.2 | `script.R` | `ar_us_canada_fig10.2.png` | `~/R/helper_function_2026.R` |
| 10.US.Canada / fig10.3 | `script.R` | `ar_us_canada_fig10.3.png` | Uses `gadm0_aqli_2024`. The script does not source a helper itself |
| Appendix | `script.R` | `ar_appendix_figa.1.png` | `~/R/helper_function_2026.R` |

Paths above are relative to `2026/annual.report.2026/`.
