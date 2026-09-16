# Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color

Replication code and processed analytic data for:

Nanum Jeon and Jennie E. Brand, "Heterogeneous Effects of Completing College on Reducing COVID-19 Job Loss by Race and Skin Color," *Demography*.

**Data:** National Longitudinal Survey of Youth 1997 (NLSY97), including Round 20 and the
COVID-19 supplement ([NLS Investigator](https://www.nlsinfo.org/investigator)).
Raw NLSY97 files are not included; the processed analytic data sets are in `intermediate/`.

## Repository structure

```
scripts/        00_setup.R and the analysis scripts (40_–49_)
src/            Helper functions, sourced automatically by 00_setup.R
intermediate/   Processed analytic data (merged_list.rds, non_imputed.rds)
data/           Data source information (no raw data)
```

## How to run

1. Download or clone this repository.
2. Open R (4.4) with the working directory set to the repository root
   (for example, open a new RStudio project here). Paths use `here::here()`.
3. Run:

```r
source("scripts/00_setup.R")   # installs/loads packages, creates results/, loads src/

# Estimate models on the imputed and non-imputed data
source("scripts/40_mediation_jobloss.R")
source("scripts/41_mediation_alt_outcome.R")
source("scripts/42_mediation_non_imputed.R")
source("scripts/43_mediation_alt_moderator.R")

# Pool and summarize results
source("scripts/44_analysis_jobloss.R")
source("scripts/45_analysis_alt_outcome.R")
source("scripts/46_analysis_intersectional.R")
source("scripts/47_analysis_alt_intersectional.R")
source("scripts/48_analysis_alt_moderator.R")
source("scripts/49_analysis_non_imputed.R")
```

Results are saved as `.rds` files in `results/`.

## Contact

Nanum Jeon (njeon@g.ucla.edu)
