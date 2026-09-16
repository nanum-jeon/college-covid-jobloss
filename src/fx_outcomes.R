# src/outcomes.R

# ---- helpers ---------------------------------------------------------------

normalize_names <- function(df) {
  names(df) <- gsub("-", "_", names(df))
  names(df) <- gsub("[.]", "_", names(df))
  df
}

# Policy knobs live here
recode_covid_flag <- function(x, zero_for_valid_skip = FALSE) {
  dplyr::case_when(
    x == 1 ~ 1,
    x == 0 ~ 0,
    x == -4 ~ if (zero_for_valid_skip) 0 else NA_real_,
    x %in% c(-1, -2, -3, -5) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

invol_from_reason <- function(reason_code, involuntary_set,
                              zero_for_valid_skip = TRUE,
                              zero_for_other_known = TRUE) {
  dplyr::case_when(
    reason_code %in% involuntary_set ~ 1L,
    reason_code %in% c(-1, -2, -3, -5) ~ NA_integer_,
    reason_code == -4 ~ if (zero_for_valid_skip) 0L else NA_integer_,
    is.na(reason_code) ~ NA_integer_,
    TRUE ~ if (zero_for_other_known) 0L else NA_integer_
  )
}

due_to_covid_from <- function(reason_code, covid_flag, involuntary_set,
                              zero_for_valid_skip = TRUE,
                              zero_for_other_known = TRUE,
                              worked_last_week = NULL) {
  invol    <- invol_from_reason(reason_code, involuntary_set,
                                zero_for_valid_skip, zero_for_other_known)
  cov_attr <- recode_covid_flag(covid_flag, zero_for_valid_skip)
  
  out <- dplyr::case_when(
    invol == 1 & cov_attr == 1 ~ 1,
    invol == 1 & cov_attr == 0 ~ 1,
    invol == 0                 ~ 0,
    invol == 1 & is.na(cov_attr) ~ 0,
    TRUE ~ NA_real_
  )
  
  # Optional override (supplement): worked last week => 0; non-interview => NA
  if (!is.null(worked_last_week)) {
    out[worked_last_week == 1]  <- 0
    out[worked_last_week == -5] <- NA_real_
  }
  out
}

# ---- components ------------------------------------------------------------

# COVID-15A block: “due to COVID” disruptions (not all are separations)
make_covid_disruption <- function(disruption) {
  disruption |>
    dplyr::mutate(
      covid_disruption = dplyr::case_when(
        .data$COVID_15A == 1 ~ 1,    # explicitly disrupted
        .data$COVID_15A == 0 ~ 0,    # explicitly not disrupted
        
        # If 15A is -4/-5, infer from sub-items:
        (.data$COVID_15A %in% c(-4, -5)) &
          (.data$COVID_15AD == 1 | .data$COVID_15AA == 1 |
             .data$COVID_15AB == 1 | .data$COVID_15AC == 1) ~ 1,
        (.data$COVID_15A %in% c(-4, -5)) &
          (.data$COVID_15AD == 0 | .data$COVID_15AA == 0 |
             .data$COVID_15AB == 0 | .data$COVID_15AC == 0) ~ 0,
        
        TRUE ~ NA_real_ # other negatives → unknown
      )
    ) |>
    dplyr::select(PUBID, covid_disruption)
}

# Supplement outcome (reason + attribution)
make_outcome_from_supplement <- function(supp, involuntary_set = c(1, 2, 5)) {
  supp |>
    dplyr::mutate(
      covid_jobloss_supplement =
        due_to_covid_from(
          reason_code = COVID_11,
          covid_flag  = COVID_11A,
          involuntary_set = involuntary_set,
          zero_for_valid_skip = TRUE,
          zero_for_other_known = TRUE,
          worked_last_week = COVID_4
        )
    ) |>
    dplyr::select(PUBID, covid_jobloss_supplement)
}

# Round 20 outcome (reason + attribution) with legacy “both -4 ⇒ 0” rule
make_outcome_from_round20 <- function(disp, involuntary_set = c(1, 2, 20)) {
  tmp <- disp |>
    dplyr::mutate(
      covid_jobloss_round_20 =
        due_to_covid_from(
          reason_code = YEMP_58400_01_2021,
          covid_flag  = YEMP_58400_COVID_01_2021,
          involuntary_set = involuntary_set,
          zero_for_valid_skip = TRUE,
          zero_for_other_known = TRUE
        )
    )
  
  dplyr::select(tmp, PUBID, covid_jobloss_round_20)
}

# ---- combiners -------------------------------------------------------------


# Displacement-only composite (involuntary-set restricted to layoff/closure)
make_covid_displacement <- function(supp, disp) {
  supp <- normalize_names(supp)
  disp <- normalize_names(disp)
  
  stopifnot(!anyDuplicated(supp$PUBID),
            !anyDuplicated(disp$PUBID))
  
  s <- make_outcome_from_supplement(supp, involuntary_set = c(1, 2))
  r <- make_outcome_from_round20(disp,  involuntary_set = c(1, 2))
  
  out <- s |>
    dplyr::left_join(r, by = "PUBID") |>
    dplyr::mutate(
      covid_displacement = dplyr::case_when(
        covid_jobloss_supplement == 1 | covid_jobloss_round_20 == 1 ~ 1,
        (!is.na(covid_jobloss_supplement) & covid_jobloss_supplement == 0) |
          (!is.na(covid_jobloss_round_20) & covid_jobloss_round_20 == 0) ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(PUBID, covid_displacement)
  
  out
}

# Illness-related job loss (narrow: illness codes only)
make_covid_illness_jobloss <- function(supp, disp) {
  supp <- normalize_names(supp)
  disp <- normalize_names(disp)
  
  stopifnot(!anyDuplicated(supp$PUBID),
            !anyDuplicated(disp$PUBID))
  
  s <- make_outcome_from_supplement(supp, involuntary_set = c(5))
  r <- make_outcome_from_round20(disp,  involuntary_set = c(20))
  
  out <- s |>
    dplyr::left_join(r, by = "PUBID") |>
    dplyr::mutate(
      covid_illness_jobloss = dplyr::case_when(
        covid_jobloss_supplement == 1 | covid_jobloss_round_20 == 1 ~ 1,
        (!is.na(covid_jobloss_supplement) & covid_jobloss_supplement == 0) |
          (!is.na(covid_jobloss_round_20) & covid_jobloss_round_20 == 0) ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(PUBID, covid_illness_jobloss)
  
  out
}

# Care-related job loss (supplement-only)
make_covid_care_jobloss <- function(supp) {
  supp <- normalize_names(supp)
  s <- make_outcome_from_supplement(supp, involuntary_set = c(6, 7))
  
  s |>
    dplyr::mutate(
      covid_care_jobloss = dplyr::case_when(
        covid_jobloss_supplement == 1 ~ 1,
        covid_jobloss_supplement == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(PUBID, covid_care_jobloss)
}




make_covid_jobloss <- function(disruption, supp, disp) {
  
  covid_19_outcomes <- disruption%>%
    left_join(supp, by = "PUBID") %>% 
    left_join(disp, by = "PUBID")
  
  # Covid-related job loss from COVID supplement survey
  covid_19_outcomes <- covid_19_outcomes %>%
   mutate(
     loss_covyr = case_when(
       COVID_11A == -4 ~ 0,
       COVID_11A == 0  ~ 0,
       COVID_11A == 1  ~ 1,
       TRUE ~ NA_real_
     )
   )


  # Covid-related job loss from 2021 survey
  covid_19_outcomes$loss_21yr <- NA
  covid_19_outcomes$loss_21yr[covid_19_outcomes$YEMP_58400_COVID_01_2021 == -4] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AA == -4] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AB == -4] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AC == -4] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AD == -4] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$YEMP_58400_COVID_01_2021 == 0] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AA == 0] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AB == 0] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AC == 0] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AD == 0] <- 0
  covid_19_outcomes$loss_21yr[covid_19_outcomes$YEMP_58400_COVID_01_2021 == 1] <- 1
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AA == 1] <- 1
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AB == 1] <- 1
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AC == 1] <- 1
  covid_19_outcomes$loss_21yr[covid_19_outcomes$COVID_15AD == 1] <- 1
  
  # Combined measure
  covid_19_outcomes <- covid_19_outcomes %>%
    mutate(
      covid_jobloss = case_when(
        loss_21yr == 0 & loss_covyr == 0  ~ 0,
        loss_21yr == 0 & loss_covyr == 1  ~ 1,
        loss_21yr == 0 & is.na(loss_covyr) ~ 0,
        loss_21yr == 1 & loss_covyr == 0  ~ 1,
        loss_21yr == 1 & loss_covyr == 1  ~ 1,
        loss_21yr == 1 & is.na(loss_covyr)  ~ 1,
        is.na(loss_21yr) & loss_covyr == 0 ~ 0,
        is.na(loss_21yr) & loss_covyr == 1 ~ 1,
        TRUE ~ NA
      )
    )
  
  covid19_jobloss <- covid_19_outcomes %>% 
    select(PUBID, covid_jobloss)
    
  return(covid19_jobloss)
}
 