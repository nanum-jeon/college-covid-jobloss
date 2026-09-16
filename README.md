# Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color

Replication code for:

Nanum Jeon and Jennie E. Brand, "Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color," *Demography*.

**Data:** National Longitudinal Survey of Youth 1997 (NLSY97), including the Round 20
and COVID-19 supplement data. **No data are included in this repository.**
NLSY97 data are available from the U.S. Bureau of Labor Statistics via
[NLS Investigator](https://www.nlsinfo.org/investigator). See `data/README.md` for
the files the code expects.

## Repository structure

```
scripts/        Numbered analysis pipeline (run in order)
src/            Helper functions, sourced automatically by scripts/00_setup.R
data/           Place NLSY97 and auxiliary data here (not included)
intermediate/   Generated respondent-level files (not included)
```

`results/`, `results/figure/`, `results/table/`, and `logs/` are created automatically.

## Pipeline

| Step | Scripts | Purpose |
|---|---|---|
| Setup | `00_setup.R` | Packages, paths, sources `src/` |
| Cleaning | `10_`–`11_` | Covariates and multiple imputation |
| Variables | `20_`–`24_` | Outcomes, treatment, mediators, moderators, analytic sample and survey designs |
| Descriptives | `30_`–`33_` | Descriptive statistics |
| Analysis | `40_`–`53_` | Mediation/effect estimation, alternative outcomes and moderators, non-imputed models, sensitivity analyses, imputation diagnostics |
| Figures | `60_`–`68_` | Figures |
| Tables | `70_`–`78_` | LaTeX tables |

## Running

1. Obtain the NLSY97 data and place the files in `data/` (see `data/README.md`).
2. Open R in the repository root (paths are resolved with `here::here()`).
3. Run the scripts in numeric order, e.g. `source("scripts/10_clean_covariates.R")`.

## Software

R (analyses run with R 4.4). Required packages are listed in `scripts/00_setup.R`
and installed automatically if missing.

## Contact

Nanum Jeon (njeon@g.ucla.edu)
