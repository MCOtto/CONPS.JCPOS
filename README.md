# CONPS.JCPOS

Germination and field-survival analysis for the Colorado Native Plant Society
study funded by a 2025 Jefferson County Parks & Open Space research grant.

| Folder | Contents |
|---|---|
| `ori/` | Files received and not modified: data workbooks, grant report, talk slides |
| `R/` | `Germination.R`, `Survival.R`, and shared functions in `Helpers.R` |
| `doc/` | Analysis documentation, original analysis notes, `images/` and `tables/` written by the scripts |
| `ref/` | Bibliography (`JCOS.bib`) and reference PDFs |

To reproduce, open the project in RStudio and run

```r
source("R/Germination.R")
source("R/Survival.R")
```

Required packages are listed under `Imports` in `DESCRIPTION`. `here::here()`
locates the project root from the `DESCRIPTION` file.

The analysis is documented in `doc/JCPOS_Germination_Survival_Analysis.docx`.
