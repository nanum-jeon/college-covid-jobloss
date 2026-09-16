# src/cleaning_covars.R

# ---- helpers ----
neg_to_na <- function(x) ifelse(x %in% -1:-5, NA, x)

cpi_to_base <- function(value, year, cpi_tbl, base_year = 2022) {
  base <- cpi_tbl$cpi[cpi_tbl$year == base_year]
  value * base / cpi_tbl$cpi[match(year, cpi_tbl$year)]
}



# ---- background covariates (your block as a function) ----
build_background_covars <- function(raw, cpi_tbl, base_year = 2022) {
  background_covars <- raw %>%
    dplyr::transmute(
      PUBID = PUBID,
      female = dplyr::case_when(
        KEY_SEX_1997 == 2 ~ 1,   # 2 → 1
        KEY_SEX_1997 == 1 ~ 0,   # 1 → 0
        TRUE              ~ NA_integer_
      ),
      maeduc = dplyr::case_when(
        `CV_HGC_BIO_MOM_1997` %in% -1:-5 | `CV_HGC_BIO_MOM_1997` == 95 ~ NA,
        TRUE ~ as.numeric(`CV_HGC_BIO_MOM_1997`)
      ),
      faeduc = dplyr::case_when(
        `CV_HGC_BIO_DAD_1997` %in% -1:-5 | `CV_HGC_BIO_DAD_1997` == 95 ~ maeduc,
        TRUE ~ as.numeric(`CV_HGC_BIO_DAD_1997`)
      ),
      rural_12 = dplyr::case_when(
        `CV_URBAN-RURAL_AGE_12_1997` == 0 ~ 1,
        `CV_URBAN-RURAL_AGE_12_1997` == 1 ~ 0,
        `CV_URBAN-RURAL_AGE_12_1997` < 0 | `CV_URBAN-RURAL_AGE_12_1997` == 2 ~ dplyr::case_when(
          `CV_URBAN-RURAL_AGE_12_YCHR_1997` == 0 ~ 1,
          `CV_URBAN-RURAL_AGE_12_YCHR_1997` == 1 ~ 0,
          TRUE ~ NA
        ),
        TRUE ~ NA
      ),
      south_12 = dplyr::case_when(
        `CV_CENSUS_REGION_AGE_12_1997` == 3 ~ 1,
        `CV_CENSUS_REGION_AGE_12_1997` %in% c(1, 2, 4) ~ 0,
        `CV_CENSUS_REGION_AGE_12_1997` < 0 ~ dplyr::case_when(
          `CV_CENSUS_REGION_AGE_12_YCHR_1997` == 3 ~ 1,
          `CV_CENSUS_REGION_AGE_12_YCHR_1997` %in% c(1, 2, 4) ~ 0,
          TRUE ~ NA
        ),
        TRUE ~ NA
      ),
      intact_12 = dplyr::case_when(
        `CV_YTH_REL_HH_AGE_12_1997` == 1 ~ 1,
        `CV_YTH_REL_HH_AGE_12_1997` %in% 2:10 ~ 0,
        TRUE ~ NA
      ),
      sibsz = neg_to_na(`P2-024_1997`),
      sibsz = ifelse(is.na(sibsz), NA, pmin(sibsz, 16)),
      delinq = dplyr::case_when(!(FP_YYCRIMI_1997 %in% -1:-5) ~ as.numeric(FP_YYCRIMI_1997),
                                TRUE ~ NA_real_),
      tchgd   = dplyr::case_when(`YSCH-36400_1997` == 1 ~ 1,
                                 `YSCH-36400_1997` %in% 2:4 ~ 0,
                                 TRUE ~ NA_real_),
      schsafe = dplyr::case_when(`YSCH-37000_1997` == 1 ~ 1,
                                 `YSCH-37000_1997` %in% 2:4 ~ 0,
                                 TRUE ~ NA_real_),
      pct_peer_about75 = dplyr::case_when(
        `YPRS-1100_1997` == 4 ~ 1,
        `YPRS-1100_1997` %in% 1:5 ~ 0,   # other valid responses
        TRUE ~ NA_integer_               # missing / invalid
      ),
      pct_peer_more_than90 = dplyr::case_when(
        `YPRS-1100_1997` == 5 ~ 1,
        `YPRS-1100_1997` %in% 1:5 ~ 0,
        TRUE ~ NA_integer_
      ),
      asvab_pst = dplyr::case_when(!(`ASVAB_MATH_VERBAL_SCORE_PCT_1999` %in% -1:-5) ~
                                     as.numeric(`ASVAB_MATH_VERBAL_SCORE_PCT_1999`)),
      asvab_pst = asvab_pst / 1000,
      subs_use  = dplyr::case_when(!(FP_YYSUBSI_1997 %in% -1:-5) ~ as.numeric(FP_YYSUBSI_1997)),
      stolen = dplyr::case_when(
        !(`YSCH-35900_1997` %in% -1:-5) & `YSCH-35900_1997` >= 1 ~ 1,
        !(`YSCH-35900_1997` %in% -1:-5) & `YSCH-35900_1997` == 0 ~ 0,
        TRUE ~ NA_integer_
      ),
      threatened = dplyr::case_when(
        !(`YSCH-36000_1997` %in% -1:-5) & `YSCH-36000_1997` >= 1 ~ 1,
        !(`YSCH-36000_1997` %in% -1:-5) & `YSCH-36000_1997` == 0 ~ 0,
        TRUE ~ NA_integer_
      ),
      fight = dplyr::case_when(
        !(`YSCH-36100_1997` %in% -1:-5) & `YSCH-36100_1997` >= 1 ~ 1,
        !(`YSCH-36100_1997` %in% -1:-5) & `YSCH-36100_1997` == 0 ~ 0,
        TRUE ~ NA_integer_
      )
    )
  
  parinc <- raw %>%
    dplyr::select(PUBID, dplyr::contains("HIU-2")) %>%
    tidyr::pivot_longer(dplyr::starts_with("HIU-2_"),
                        names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(
      Year  = as.numeric(stringr::str_extract(Year, "\\d{4}")) - 1,
      Value = ifelse(Value < 0, NA, Value),
      year_parinc = cpi_to_base(Value, Year, cpi_tbl, base_year)
    ) %>%
    dplyr::group_by(PUBID) %>%
    dplyr::summarise(parinc = mean(year_parinc, na.rm = TRUE), .groups = "drop") %>%
    tidyr::replace_na(list(parinc = NA)) %>%
    dplyr::mutate(parinc = parinc / 1000)
  
  col_prep <- raw %>%
    dplyr::select(PUBID, dplyr::contains("YSCH-31700"), dplyr::contains("YSCH-21625")) %>%
    tidyr::pivot_longer(cols = -PUBID, names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(
      Year  = as.numeric(substr(Year, nchar(Year) - 3, nchar(Year))),
      Value = dplyr::if_else(Value < 0, NA_real_, as.numeric(Value))
    ) %>%
    dplyr::group_by(PUBID)  %>%                       
    dplyr::summarise(
      any_nonmis = any(!is.na(Value)),
      col_prep   = dplyr::if_else(any_nonmis, 
                                  if_else(any(Value == 2, na.rm = TRUE), 1, 0), 
                                  NA_integer_),
      .groups = "drop"
    ) %>%
    dplyr::select(PUBID, col_prep)
  
  self_reported_hs_gpa <- raw %>%
    dplyr::select(PUBID, dplyr::contains("YSCH-7300")) %>%
    tidyr::pivot_longer(cols = -PUBID, names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(
      Year  = as.numeric(substr(Year, nchar(Year) - 3, nchar(Year))),
      Value = ifelse(Value < 0, NA, Value),
      self_reported_hs_gpa = dplyr::case_when(
        Value == 2  ~ 1.0,
        Value == 3  ~ 1.7,
        Value == 5  ~ 2.0,
        Value == 6  ~ 3.0,
        Value == 7  ~ 3.3,
        Value == 8  ~ 4.0,
        Value == 10 ~ 3.0,
        Value == 11 ~ 2.0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(PUBID) %>%
    dplyr::summarise(self_reported_hs_gpa = mean(self_reported_hs_gpa, na.rm = TRUE), .groups = "drop") %>%
    tidyr::replace_na(list(self_reported_hs_gpa = NA))
  
  hs_gpa <- raw %>%
    dplyr::select(PUBID, `TRANS_CRD_GPA_OVERALL_HSTR`) %>%
    dplyr::mutate(hs_gpa = dplyr::case_when(
      `TRANS_CRD_GPA_OVERALL_HSTR` < 0 ~ NA_real_,
      TRUE ~ `TRANS_CRD_GPA_OVERALL_HSTR` / 100
    )) %>%
    dplyr::left_join(self_reported_hs_gpa, by = "PUBID") %>%
    dplyr::mutate(hs_gpa = dplyr::case_when(
      is.na(hs_gpa) ~ self_reported_hs_gpa,
      TRUE ~ hs_gpa
    )) %>%
    dplyr::select(PUBID, hs_gpa)
  
  background_covars %>%
    dplyr::left_join(parinc,   by = "PUBID") %>%
    dplyr::left_join(col_prep, by = "PUBID") %>%
    dplyr::left_join(hs_gpa,   by = "PUBID")
}

# ---- family factors (kept here if/when you need) ----
build_family_factors <- function(raw, age_last, dob_m) {
  cohabited_18 <- raw %>%
    dplyr::select(PUBID, CVC_FIRST_COHAB_MONTH) %>%
    dplyr::left_join(age_last, by = "PUBID") %>%
    dplyr::left_join(dob_m,    by = "PUBID") %>%
    dplyr::mutate(cohabited_18 = dplyr::case_when(
      age_last >= 18 & CVC_FIRST_COHAB_MONTH == -4 ~ FALSE,
      age_last >= 18 & CVC_FIRST_COHAB_MONTH > 0 & ((CVC_FIRST_COHAB_MONTH - dob_m) / 12) <= 18 ~ 1,
      age_last >= 18 & CVC_FIRST_COHAB_MONTH > 0 & ((CVC_FIRST_COHAB_MONTH - dob_m) / 12) > 18  ~ 0,
      TRUE ~ NA
    )) %>%
    dplyr::select(PUBID, cohabited_18)
  
  child_18 <- raw %>%
    dplyr::select(PUBID, dplyr::contains("CV_CHILD_BIRTH_MONTH.01")) %>%
    tidyr::pivot_longer(cols = -PUBID, names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(
      Year  = as.numeric(substr(Year, nchar(Year) - 3, nchar(Year))),
      Value = dplyr::if_else(Value < 0, NA_real_, as.numeric(Value))
    ) %>%
    dplyr::left_join(age_last, by = "PUBID") %>%
    dplyr::left_join(dob_m,    by = "PUBID") %>%
    dplyr::mutate(
      yearly_child_age18 = dplyr::case_when(
        age_last >= 18 & ((Value - dob_m)/12) <= 18 ~ 1,
        age_last >= 18 & ((Value - dob_m)/12) >  18 ~ 0,
        TRUE ~ NA     # (age_last < 18) 또는 정보 없음 -> NA 유지
      )
    ) %>%
    dplyr::group_by(PUBID) %>%
    dplyr::summarise(
      any_nonmis = any(!is.na(yearly_child_age18)),
      child_18 = dplyr::if_else(
        any_nonmis,
        as.integer(any(yearly_child_age18, na.rm = TRUE)),  # TRUE→1, FALSE→0
        NA_integer_                                      # 전부 NA면 NA
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(PUBID, child_18)
  
  dplyr::left_join(cohabited_18, child_18, by = "PUBID")
}
