# intermediate/

Processed analytic data (N = 8,984 NLSY97 respondents) used by the analysis scripts.

| File | Contents | Used by |
|---|---|---|
| `merged_list.rds` | List of 5 multiply imputed analytic data sets | `40_`, `41_`, `43_mediation_alt_moderator.R` |
| `non_imputed.rds` | Non-imputed analytic data set | `42_mediation_non_imputed.R` |

Variables include background covariates, college completion by age 25 (`compcoll25`),
high-skill occupation in 2019 (`hiskil19`), COVID-19 job loss outcomes, race and
skin-tone measures, and NLSY97 survey design variables (`VPSU`, `VSTRAT`, `any_weight`).
