# Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color

Replication code and processed analytic data for:

Nanum Jeon and Jennie E. Brand, "Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color," *Demography*.

**Data:** National Longitudinal Survey of Youth 1997 (NLSY97), including Round 20 and the
COVID-19 supplement ([NLS Investigator](https://www.nlsinfo.org/investigator)).
Raw NLSY97 files are not included; the processed analytic data are in `intermediate/`.

## Repository structure

```
scripts/        00_setup.R and the main analysis scripts
src/            Helper functions, sourced automatically by 00_setup.R
intermediate/   Processed analytic data (merged_list.rds)
data/           Data source information (no raw data)
```

## How to run

1. Download or clone this repository.
2. Open R (4.4) with the working directory set to the repository root
   (for example, open a new RStudio project here). Paths use `here::here()`.
3. Run:

```r
source("scripts/00_setup.R")   # installs/loads packages, creates results/, loads src/

source("scripts/40_mediation_jobloss.R")  # fits mediation models on the 5 imputed data sets
source("scripts/44_analysis_jobloss.R")   # pools results: main and mechanism analyses
```

Output: `results/covid_jobloss_list.rds` (model estimates) and
`results/covid_jobloss_results.rds` (pooled main and mechanism results).

## Contact

Nanum Jeon (njeon@g.ucla.edu)
