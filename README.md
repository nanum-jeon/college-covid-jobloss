# Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color

Replication code and processed analytic data for:

Nanum Jeon and Jennie E. Brand, "Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color," *Demography*.

**Data:** National Longitudinal Survey of Youth 1997 (NLSY97), including Round 20 and the
COVID-19 supplement ([NLS Investigator](https://www.nlsinfo.org/investigator)).
Raw NLSY97 files are not included; the processed analytic data are in `intermediate/`.

## Repository structure

```
scripts/
  00_setup.R              Loads packages, sets paths, sources src/
  40_mediation_jobloss.R  Fits the mediation models on the 5 imputed data sets
  44_analysis_jobloss.R   Pools estimates across imputations: main results
src/
  mediation.R             fit_mediation_models() (EIF-based estimators with SuperLearner)
  analysis.R              Pooling and formatting functions
intermediate/
  merged_list.rds         Processed analytic data (5 multiply imputed data sets)
data/                     Data source information (no raw data)
```

## How to run

1. Download or clone this repository.
2. Open R (4.4) with the working directory set to the repository root
   (for example, open a new RStudio project here). Paths use `here::here()`.
3. Run:

```r
source("scripts/00_setup.R")
source("scripts/40_mediation_jobloss.R")
source("scripts/44_analysis_jobloss.R")
```

Output: `results/covid_jobloss_list.rds` (model estimates) and
`results/covid_jobloss_results.rds` (pooled main results).

## Contact

Nanum Jeon (njeon@g.ucla.edu)
